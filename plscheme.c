/*
 * PL/scheme, Procedural Language Handler
 * $Id: plscheme.c,v 1.25 2008/02/24 09:38:24 knt Exp $
 *
 * Copyright (c) 2006, Volkan YAZICI <yazicivo@ttnet.net.tr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "postgres.h"
#include "fmgr.h"
#include "funcapi.h"
#include "lib/stringinfo.h"
#include "commands/trigger.h"
#include "access/heapam.h"
#include "utils/syscache.h"
#include "catalog/pg_proc.h"
#include "catalog/pg_type.h"
#include "utils/builtins.h"
#include "utils/lsyscache.h"
#include "utils/typcache.h"
#include "utils/array.h"
#include "utils/fmgroids.h"
#include "utils/memutils.h"
#include "nodes/makefuncs.h"
#include "parser/parse_type.h"
#include "executor/spi.h"
#include "miscadmin.h"

#include "libguile.h"


PG_MODULE_MAGIC;


/*
 * Storage types for procedure meta information.
 */

typedef struct arg_t
{
	bool	 isrow;
	char	*name;
	char	 mode;
	bool	 isnull;

	/* If this is a basic type. */
	Oid		 type;
	char	*typename;
	int		 typemod;
	Oid		 typeioparam;
	regproc  inputfn;
	char	*value;

	/* If this is a complex type that's holding an inner tuple. */
	int				 nargs;
	struct arg_t	*args;
} arg_t;

/* Trigged when? */
typedef enum
{
	TG_BEFORE		= 0,
	TG_AFTER		= 1
} tg_when_t;

/* Trigged on what? */
typedef enum
{
	TG_ON_INSERT	= 2,
	TG_ON_DELETE	= 3,
	TG_ON_UPDATE	= 4
} tg_event_t;

/* Trigged for what? */
typedef enum
{
	TG_FOR_ROW		= 5,
	TG_FOR_STMT		= 6
} tg_for_t;

/* Procedure meta information. */
typedef struct
{
	Oid		 id;
	char	*name;
	char	*src;
	bool	 isreadonly;
	bool	 istrigger;

	/* Procedure arguments. */
	int		 nargs;
	arg_t	*args;

	/* Trigger arguments. */
	Oid			  tg_relid;
	char		 *tg_name;
	char		 *tg_relname;
	char		 *tg_tablename;
	char		 *tg_tableschema;
	tg_when_t	  tg_when;
	tg_event_t	  tg_event;
	tg_for_t	  tg_for;
	int			  tg_newtup_natts;
	int			  tg_oldtup_natts;
	arg_t		 *tg_newtup;
	arg_t		 *tg_oldtup;
	int			  tg_nargs;
	char		**tg_args;

	/* SRF information. */
	ReturnSetInfo	*rs_info;

	/* Result information. */
	Oid		restype;
	bool	resisset;
	bool	resisrow;
	bool	resisnull;
	Datum	res;
	
	/* Transaction and command IDs. (To check validity of a cached result.) */
	TransactionId	xmin;
	CommandId		cmin;
} proc_t;

/* Storage type for hash_t. */
typedef int	hash_t;

/* Structure for storing cached proc_t information. */
typedef struct cache_node_t
{
	/* Hash value of the procedure (OID + arguments). */
	hash_t	hash;

	/* How many times did we get called? */
	int	ncalls;
	
	/* Actual procedure meta-information. */
	proc_t	*proc;

	/* Memory context for above proc_t. */
	MemoryContext	mcxt;
	
	/* Next node. */
	struct cache_node_t	*next;
} cache_node_t;


/*
 * Global variables and definitions.
 */

/* We'll use this information to switch back to procedure's memory context. */
static MemoryContext	current_mcxt;

/*
 * Static variables to store root and getting cached node and
 * current cache size.
 */
static TransactionId	 current_transid = -1;
static int				 cache_size;
static cache_node_t		*cache_nodes;
static cache_node_t		*getting_cached_node;

/* Maximum number of (non-volatile and non-SRF) procedures to cache. */
#ifndef CACHE_MAX_SIZE
#define CACHE_MAX_SIZE 64
#endif
static int cache_max_size = CACHE_MAX_SIZE;

/* Hashing macros. (With buggy a hash_append() implementation.) */
#define hash_init()				0
#define hash_append(old, new)	old = ((old | new) ^ (new >> 4)) & 0xFF

/* Caller should foresee below buggy "unsigned char *" casting's result. */
#define hash_buffer(buf, len)	((hash_t) crc32((unsigned char *) buf, len))

/* Buffer size will be used in snprintf()'ing arg_t/proc_t members. */
#define ARG_T_HASH_BUFFER_SIZE	128
#define ARG_T_HASH_NAME_SIZE	"32"
#define PROC_T_HASH_BUFFER_SIZE	512

#define CACHE_INIT() \
	/* Reset current cache node. */ \
	getting_cached_node = NULL; \
	\
	/*
	 * Reset current_transid, cache_size and cache_nodes pointer that is left
	 * from previous [top] transaction.
	 */ \
	if (current_transid != GetTopTransactionId()) \
	{ \
		current_transid = GetTopTransactionId(); \
		cache_size = 0; \
		cache_nodes = NULL; \
	} \
	\
	/* Caching is available only for non-volatile and non-SRF procedures. */ \
	if (proc->isreadonly && \
		(proc->istrigger || (!proc->istrigger && !proc->resisset))) \
	{ \
		/* Check current procedure in the cache. */ \
		if (cache_lookup()) \
		{ \
			/*
			 * Looks like we've found one. cache_lookup() should have made
			 * necessary loadings from cached proc_t into our current proc_t.
			 * We can return safely.
			 */ \
			\
			/* Don't forget to release previously looked at pg_proc tuple. */ \
			ReleaseSysCache(proctup); \
			\
			return; \
		} \
		\
		/* We'll need to register this function. */ \
		else \
			cache_register_meta(); \
	}

#define CACHE_FINALIZE() \
	/* 
	 * If we're getting cached, finalize caching by storing produced result in
	 * the cached proc_t node.
	 */ \
	if (getting_cached_node) \
		cache_register_res();

/* Procedure meta information. */
static proc_t	*proc;

/* Initialization level related variables. */
static int	guile_init_level = 0;

/*
 * Stack in the context of related exception throw, will be saved in this
 * variable using pre-unwind exception handlers. And later we'll use this
 * information to display backtrace information.
 */
static SCM  guile_backtrace_stack;

#define CALL_PROC_WITH_CATCH(proc, arg) \
	scm_c_catch(SCM_BOOL_T, \
				(scm_t_catch_body) proc, arg, \
				(scm_t_catch_handler) guile_exception_handler, NULL, \
				(scm_t_catch_handler) guile_pre_unwind_handler, NULL)

/* We'll use a safe R5RS module while executing user space code. */
static SCM	guile_execution_module = NULL;

/*
 * Wrap scm_c_define() and scm_define() calls to automatically get called
 * under guile_execution_module scope.
 */
#define scm_c_define(name, value) \
	scm_c_module_define(guile_execution_module, name, value)

#define scm_define(sym, value) \
	scm_module_define(guile_execution_module, sym, value)

/* String buffer for storing soft-port redirections. */
static StringInfo	soft_port_si;

/* Data structure for stroing exceptions' information. */
typedef struct
{
	char	*name;
	int		 code;
	SCM		 sym;
} exception_t;

/* Exceptions table. */
static exception_t	excptbl[179];

/*
 * PostgreSQL exception catching macros to use with SPI function calls. In case
 * of any error, catched exception will be passed to Guile exception handlers.
 */
#define GUILE_TRY() \
	do { \
		MemoryContext	oldcontext = CurrentMemoryContext; \
		ResourceOwner	oldowner = CurrentResourceOwner; \
		\
		/*
		 * Execute the query inside a sub-transaction, so we can cope with
		 * errors sanely.
		 */ \
		BeginInternalSubTransaction(NULL); \
		\
		/* Want to run inside function's memory context */ \
		MemoryContextSwitchTo(oldcontext); \
		\
		PG_TRY(); \
		{

#define GUILE_END_TRY() \
			/* Commit the inner transaction, return to outer xact context. */ \
			ReleaseCurrentSubTransaction(); \
			MemoryContextSwitchTo(oldcontext); \
			CurrentResourceOwner = oldowner; \
			\
			/*
			 * AtEOSubXact_SPI() should not have popped any SPI context, but
			 * just in case it did, make sure we remain connected.
			 */ \
			SPI_restore_connection(); \
		} \
		PG_CATCH(); \
		{ \
			ErrorData	*edata; \
			\
			/* Save error info. */ \
			MemoryContextSwitchTo(oldcontext); \
			edata = CopyErrorData(); \
			FlushErrorState(); \
			\
			/* Abort the inner transaction. */ \
			RollbackAndReleaseCurrentSubTransaction(); \
			MemoryContextSwitchTo(oldcontext); \
			CurrentResourceOwner = oldowner; \
			\
			/*
			 * If AtEOSubXact_SPI() popped any SPI context of the subxact, it
			 * will have left us in a disconnected state.  We need this hack to
			 * return to connected state.
			 */ \
			SPI_restore_connection(); \
			\
			/* Let Guile exception handlers finish remaining work. */ \
			scm_throw(convert_from_errcode(edata->sqlerrcode), \
					  scm_list_1(scm_from_locale_string(edata->message))); \
		} \
		PG_END_TRY(); \
	} while (0)

/*
 * Duplicate SCM of string type returned by Guile using current memory
 * context. (This will guarantee the existence related memory
 * chunk that's about to be cleaned in the future by Guile GC.)
 */
#define SCM_SAFE_STRDUP(s_scm, p) \
	do { \
		char *s = scm_to_locale_string(s_scm); \
		\
		PG_TRY(); \
		{ \
			p = pstrdup(s); \
			free(s); \
		} \
		PG_CATCH(); \
		{ \
			free(s); \
			PG_RE_THROW(); \
		} \
		PG_END_TRY(); \
	} while (0)

#define scm_is_list(p)		scm_is_true(scm_list_p(p))
#define scm_is_exact(p)		scm_is_true(scm_exact_p(p))
#define SCM_VALIDATE_EXACT(pos, num) \
	SCM_ASSERT_TYPE(scm_is_exact(num), num, pos, FUNC_NAME, "exact");

/* Module directory for initialization and data conversion scripts. */
#ifndef MODULE_DIR
#error "You must specify a full path for MODULE_DIR."
#endif
static char *module_dir = MODULE_DIR;

/* Initialization and data conversion files' names. */
#define FILE_INIT		"init.scm"
#define FILE_DATACONV	"dataconv.scm"

/* Generic function to concatenate module_dir and file names. */
#define BUILD_FILE_PATH(buf, file) \
	do { \
		int len = strlen(module_dir) + strlen(file) + 2; \
		\
		buf = palloc(len * sizeof(char)); \
		sprintf(buf, "%s/%s", module_dir, file); \
	} while (0)

#define SCM_DATA_IMPOSE_FUNC	"pl-data-impose"
#define SCM_DATA_EXPOSE_FUNC	"pl-data-expose"

static SCM	scm_data_impose;
static SCM	scm_data_expose;

#define SCM_DATA_IMPOSE(str_value, typename) \
	scm_call_2(scm_data_impose, \
			   scm_from_locale_string(str_value), \
			   scm_from_locale_string(typename))

#define SCM_DATA_EXPOSE(scm_value) \
	scm_call_1(scm_data_expose, scm_value)

/* Indice values for report_level table. */
enum
{
	REPORT_LEVEL_DEBUG = 0,
	REPORT_LEVEL_LOG,
	REPORT_LEVEL_INFO,
	REPORT_LEVEL_NOTICE,
	REPORT_LEVEL_WARNING,
	REPORT_LEVEL_EXCEPTION
};

/* Actual corresponding error levels of the above items. */
static int report_level[] = {
	DEBUG1,
	LOG,
	INFO,
	NOTICE,
	WARNING,
	ERROR
};

/* Indice values for spisym table. */
enum
{
	/* SPI execution return values. */
	SPI_SYM_OK_SELECT = 0,
	SPI_SYM_OK_SELINTO,
	SPI_SYM_OK_DELETE,
	SPI_SYM_OK_DELETE_RETURNING,
	SPI_SYM_OK_INSERT,
	SPI_SYM_OK_INSERT_RETURNING,
	SPI_SYM_OK_UPDATE,
	SPI_SYM_OK_UPDATE_RETURNING,
	SPI_SYM_OK_UTILITY,
	SPI_SYM_ERROR_ARGUMENT,
	SPI_SYM_ERROR_COPY,
	SPI_SYM_ERROR_CURSOR,
	SPI_SYM_ERROR_TRANSACTION,
	SPI_SYM_ERROR_OPUNKNOWN,
	SPI_SYM_ERROR_UNCONNECTED
};

/*
 * Scheme side symbols for SPI return values and some
 * trigger attributes.
 */
static SCM spisym[15];
static SCM tgsym[7];

/*
 * This flag will save us from free'ing already free'ed SPI plans (by
 * SPI_finish()) in the Guile garbage collection.
 */
static bool spi_conn_established = false;

#define ESTABLISH_SPI_CONN() \
	if (SPI_connect() != SPI_OK_CONNECT) \
		elog(ERROR, "Could not connect to SPI manager!"); \
	else \
		spi_conn_established = true;

#define CLOSE_SPI_CONN() \
	do { \
		SPI_finish(); \
		spi_conn_established = false; \
	} while (0)

typedef struct
{
	void		*plan;
	int			 nargs;
	Oid			*typinputs;
	Oid			*typioparams;
} spi_plan_t;

static scm_t_bits typ_spi_plan;

#define SCM_VALIDATE_SPI_PLAN(plan) \
	scm_assert_smob_type(typ_spi_plan, plan)

/*
 * We'll use this struct while forming tuple from SCM values.
 * HeapTuple/TupleDesc isn't suitable for our needs because it doesn't
 * support an easy way to extract mode/type information.
 */
typedef struct
{
	bool	  isnull;
	int		  natts;
	Oid		 *types;
	char	 *modes;
	char	**names;
	bool	 *nulls;
	Datum	 *values;
} tuple_t;


static void		init_proc_tuple(FunctionCallInfo fcinfo,
								HeapTuple *in_proctup,
								Form_pg_proc *in_procstruct);
static void		plscheme_func_handler(FunctionCallInfo fcinfo);
static void		plscheme_trig_handler(FunctionCallInfo fcinfo);

static bool		arg_t_input_overlaps(arg_t *u, int l, arg_t *v, int k);
static bool		cache_overlaps(proc_t *cached);
static void		cache_load(proc_t *cached);
static bool		cache_lookup(void);
static arg_t	*duplicate_arg_t(arg_t *args, int nargs);
static int		crc32(unsigned char *buf, int len);
static hash_t	hash_arg_t(arg_t *args, int nargs);
static hash_t	hash_proc_t(proc_t *p);
static void		cache_unregister_deprecated(void);
static void		cache_register_meta(void);
static void		cache_register_res(void);
static void		cache_free(cache_node_t *node);
static void		cache_unregister(cache_node_t *node);

static arg_t	*build_arg_t(int nargs, Oid *types, char **names,
							 char *modes, bool *nulls, Datum *values);
static arg_t	*arg_t_from_tuple(HeapTuple tup, TupleDesc tupdesc,
								  int *in_nargs);
static void		parse_func_args(FunctionCallInfo fcinfo,
								HeapTuple proctup,
								Form_pg_proc procstruct);
static void		parse_trig_args(FunctionCallInfo fcinfo);
static SCM		convert_from_errcode(int sqlerrcode);
static SCM		alist_from_arg_t(arg_t *args, int nargs,
								 char discard_mode, bool inner);
static SCM		place_func_args(SCM data);
static SCM		place_trig_args(SCM data);
static void		check_tuple_consistency(tuple_t *tuple, arg_t *args,
										int nargs);
static TupleDesc	build_tupledesc(int nelems, Oid *_types,
									char *modes, char **names);
static HeapTuple	heap_from_tuple_t(tuple_t *tuple, TupleDesc tupdesc);
static tuple_t	*tuple_t_from_alist(SCM alist, char mode, bool inner,
									arg_t *args, int nargs);
static arg_t	*extract_wrt_mode(arg_t *in_args, int in_nargs,
								  char mode, int *nargs);
static Datum	datum_from_func_res(SCM res, bool *resisnull, int *nargs,
									arg_t *args, TupleDesc tupdesc);
static SCM		handle_func_res(void *data);
static SCM		handle_trig_res(void *data);

static SCM		soft_port_handler_c(SCM in_c);
static SCM		soft_port_handler_s(SCM in_s);
static SCM		soft_port_create(void);

static void		guile_init(void);
static SCM		guile_init_execution_module(void *data);
static SCM		guile_init_post(void *data);
static void		guile_init_fly(void);
static void		guile_cleanup(void);
static SCM		guile_pre_unwind_handler(void *data, SCM key, SCM params);
static SCM		guile_exception_handler(void *data, SCM tag, SCM args);
static SCM		guile_eval_str(void *thunk);
static SCM		report(SCM in_level, SCM in_msg, SCM in_hint);

static SCM		spi_plan_mark(SCM plan);
static size_t	spi_plan_free(SCM plan);
static SCM		spi_init(SCM data);
static SCM		convert_from_spi(int ret);
static SCM		spi_return_tuples(int ret, SCM throw_args);
static SCM		spi_execute(SCM in_command, SCM in_count);
static SCM		spi_prepare(SCM in_command, SCM in_argtypes);
static SCM		spi_execute_prepared(SCM in_spiplan, SCM in_args,
									 SCM in_count);

#ifdef SAFE_R5RS
PG_FUNCTION_INFO_V1(plscheme_call_handler);
Datum plscheme_call_handler(PG_FUNCTION_ARGS)
#else
PG_FUNCTION_INFO_V1(plschemeu_call_handler);
Datum plschemeu_call_handler(PG_FUNCTION_ARGS)
#endif
{
	/* Allocate space for current procedure's meta-information. */
	proc = palloc(sizeof(proc_t));
	
	PG_TRY();
	{
		if (CALLED_AS_TRIGGER(fcinfo))
			plscheme_trig_handler(fcinfo);
		else
			plscheme_func_handler(fcinfo);
	}
	PG_CATCH();
	{
		/*
		 * Call Guile clean-up routines if necessary. (See guile_init() for
		 * more information about the initialization levels.)
		 */
		if (guile_init_level == 3)
			guile_cleanup();

		PG_RE_THROW();
	}
	PG_END_TRY();

	/*
	 * In case of a NULL return. (Trigger protocol allows function to return a
	 * null pointer, but NOT to set the isnull result flag.)
	 */
	if (proc->resisnull && !proc->istrigger)
		fcinfo->isnull = true;
	
	return proc->res;
}


/*
 * _PG_init - Backend initialization routines.
 */
void
_PG_init(void)
{
/*
 * In untrusted PL mode, we'll append a `u' character to the variable names we
 * used. This will make both plscheme and plschemeu work together without any
 * collision.
 */
#ifdef SAFE_R5RS
#define UNTRUSTED_PL_INDICATOR ""
#else
#define UNTRUSTED_PL_INDICATOR "u"
#endif

	/*
	 * Define custom GUC variables.
	 */
	DefineCustomStringVariable("plscheme" UNTRUSTED_PL_INDICATOR ".module_dir",
							   "Module directory for initialization and data "
							   "conversion scripts.",
							   NULL, &module_dir, PGC_BACKEND, NULL, NULL);
	
	DefineCustomIntVariable("plscheme" UNTRUSTED_PL_INDICATOR ".cache_max_size",
							"Maximum number of (non-volatile and non-SRF) "
							"procedures to cache.",
							NULL, &cache_max_size, 0, 1024, PGC_BACKEND,
							NULL, NULL);
}


/*
 * init_proc_tuple - Read required attributes from pg_proc for the
 * 					 procedure and return proctup and procstruct.
 */
static void
init_proc_tuple(FunctionCallInfo fcinfo, HeapTuple *in_proctup,
				Form_pg_proc *in_procstruct)
{
	HeapTuple		proctup;
	Form_pg_proc	procstruct;
	Datum			procsrc;
	bool			isnull;

	proctup = SearchSysCache(PROCOID,
							 ObjectIdGetDatum(fcinfo->flinfo->fn_oid),
							 0, 0, 0);
	if (!HeapTupleIsValid(proctup))
		elog(ERROR, "Cache lookup failed for the procedure! "
					"(OID: %u)", fcinfo->flinfo->fn_oid);
	procstruct = (Form_pg_proc) GETSTRUCT(proctup);

	/* Procedure id, name and isreadonly (volatility) attribute. */
	proc->id = fcinfo->flinfo->fn_oid;
	proc->name = NameStr(procstruct->proname);
	proc->isreadonly = (procstruct->provolatile != PROVOLATILE_VOLATILE);
	proc->istrigger = CALLED_AS_TRIGGER(fcinfo);

	/* Transaction and command IDs. */
	proc->xmin = HeapTupleHeaderGetXmin(proctup->t_data);
	proc->cmin = HeapTupleHeaderGetCmin(proctup->t_data);

	/*
	 * Procedure source code.
	 */
	procsrc = SysCacheGetAttr(PROCOID, proctup,
							  Anum_pg_proc_prosrc, &isnull);
	if (isnull)
		elog(ERROR, "Null prosrc!");
	proc->src = DatumGetCString(DirectFunctionCall1(textout, procsrc));

	*in_proctup = proctup;
	*in_procstruct = procstruct;
}


/*
 * plscheme_func_handler - Custom function call handler.
 */
static void
plscheme_func_handler(FunctionCallInfo fcinfo)
{
	HeapTuple		proctup;
	Form_pg_proc	procstruct;
	HeapTuple		typetup;
	Form_pg_type	typestruct;
	SCM				res;

	/* Read related pg_proc tuple. */
	init_proc_tuple(fcinfo, &proctup, &procstruct);
	
	/*
	 * Return type information.
	 */
	proc->restype = procstruct->prorettype;
	proc->resisset = procstruct->proretset;
	if (proc->resisset)
		proc->rs_info = (ReturnSetInfo *) fcinfo->resultinfo;
	
	typetup = SearchSysCache(TYPEOID,
							 ObjectIdGetDatum(procstruct->prorettype),
							 0, 0, 0);
	if (!HeapTupleIsValid(typetup))
		elog(ERROR, "Cache lookup failed for type %u.",
			 procstruct->prorettype);
	typestruct = (Form_pg_type) GETSTRUCT(typetup);
	proc->resisrow = (typestruct->typtype == 'c' ||
					 procstruct->prorettype == RECORDOID);
	ReleaseSysCache(typetup);
	
	/* Parse arguments. */
	parse_func_args(fcinfo, proctup, procstruct);

	CACHE_INIT();

	/* We must initialized Guile just before Scheme side assignments. */
	guile_init();

	/*
	 * Place parsed procedure arguments into the Scheme code and
	 * initialize Scheme side of the SPI stuff.
	 */
	CALL_PROC_WITH_CATCH(place_func_args, NULL);
	CALL_PROC_WITH_CATCH(spi_init, NULL);

	ESTABLISH_SPI_CONN();
	
	/* Execute related Scheme code. */
	res = CALL_PROC_WITH_CATCH(guile_eval_str, (void *) proc->src);

	CLOSE_SPI_CONN();
	
	/*
	 * Receive and process returned result. (We need to process returned result
	 * after closing SPI connection. Because SPI_finish() will reset the memory
	 * context it's within.)
	 */
	CALL_PROC_WITH_CATCH(handle_func_res, (void *) res);

	/* Release procedure's pg_proc tuple. */
	ReleaseSysCache(proctup);

	/* Make necessary cleanups. */
	guile_cleanup();

	CACHE_FINALIZE();
}


/*
 * plscheme_trig_handler - Trigger handler.
 */
static void
plscheme_trig_handler(FunctionCallInfo fcinfo)
{
	HeapTuple		proctup;
	Form_pg_proc	procstruct;
	SCM				res;

	/* Read related pg_proc tuple. */
	init_proc_tuple(fcinfo, &proctup, &procstruct);
	
	/* Parse trigger arguments. */
	parse_trig_args(fcinfo);

	CACHE_INIT();

	/* Initialize Guile. */
	guile_init();
	
	/*
	 * Place parsed trigger arguments and initialize SPI in
	 * its own `catch' block.
	 */
	CALL_PROC_WITH_CATCH(place_trig_args, NULL);
	CALL_PROC_WITH_CATCH(spi_init, NULL);
	
	ESTABLISH_SPI_CONN();
	
	/* Execute related Scheme code. */
	res = CALL_PROC_WITH_CATCH(guile_eval_str, (void *) proc->src);

	CLOSE_SPI_CONN();
	
	/* Process returned result. */
	CALL_PROC_WITH_CATCH(handle_trig_res, (void *) res);

	/* Release procedure's pg_proc tuple. */
	ReleaseSysCache(proctup);

	/* Make necessary cleanups. */
	guile_cleanup();

	CACHE_FINALIZE();
}


/*
 * arg_t_input_overlaps - Checks if specified arg_t structs' input arguments
 * 						  (that are arguments of IN/INOUT mode) overlaps.
 */
static bool
arg_t_input_overlaps(arg_t *u, int l, arg_t *v, int k)
{
	bool	ret = (l == k);
	int		i;

	for (i = 0; ret && i < l; i++)
	{
		/* Skip arguments of OUT mode. */
		if (u[i].mode == PROARGMODE_OUT)
			continue;

		/* First, compare basic attributes. */
		ret = (u[i].isrow == v[i].isrow &&
			   u[i].type == v[i].type &&
			   u[i].mode == v[i].mode &&
			   u[i].isnull == v[i].isnull &&
			   !strcmp(u[i].name, v[i].name));

		/* If succesfull so far, continue with checking attribute values.*/
		if (ret)
		{
			/* If this holds a nested arg_t, check them too. */
			if (u[i].isrow)
				ret = arg_t_input_overlaps(u[i].args, u[i].nargs,
										   v[i].args, v[i].nargs);

			/* This holds a basic type, just check values. */
			else
				ret = !strcmp(u[i].value, v[i].value);
		}
	}

	return ret;
}


/*
 * cache_overlaps - Check whether current procedure meta-information overlaps
 *					with the specified proc_t or not.
 */
static bool
cache_overlaps(proc_t *cached)
{
	bool	ret;
	
	/*
	 * Trigger procedure.
	 */
	if (cached->istrigger)
	{
		/* First compare basic attributes. */
		ret = (cached->tg_when == proc->tg_when &&
			   cached->tg_event == proc->tg_event &&
			   cached->tg_for == proc->tg_for);

		/* Compare NEW/OLD tuples. */
		if (ret && cached->tg_for == TG_FOR_ROW)
			switch (cached->tg_event)
			{
				case TG_ON_INSERT:
					ret = arg_t_input_overlaps(cached->tg_newtup,
											   cached->tg_newtup_natts,
											   proc->tg_newtup,
											   proc->tg_newtup_natts);
					break;

				case TG_ON_DELETE:
					ret = arg_t_input_overlaps(cached->tg_oldtup,
											   cached->tg_oldtup_natts,
											   proc->tg_oldtup,
											   proc->tg_oldtup_natts);
					break;

				case TG_ON_UPDATE:
					ret = arg_t_input_overlaps(cached->tg_newtup,
											   cached->tg_newtup_natts,
											   proc->tg_newtup,
											   proc->tg_newtup_natts);
					ret = arg_t_input_overlaps(cached->tg_oldtup,
											   cached->tg_oldtup_natts,
											   proc->tg_oldtup,
											   proc->tg_oldtup_natts);
					break;
			}
	}

	/*
	 * Normal procedure call.
	 */
	else
		/* Just an input argument check is enough. */
		ret = arg_t_input_overlaps(cached->args, cached->nargs,
								   proc->args, proc->nargs);
	
	return ret;
}


/*
 * cache_load - Loads given cached result into the current proc_t.
 * 				(Only necessarily required attributes during a procedure
 * 				return will get loaded.)
 */
static void
cache_load(proc_t *cached)
{
	/*
	 * resisnull and res attributes are enough for a non-volatile
	 * (and non-SRF) procedure.
	 */
	
	/* Copy isnull attribute. */
	proc->resisnull = cached->resisnull;

	/* Copy the result Datum. */
	proc->res = cached->res;
}


/*
 * cache_lookup - Look up for the current procedure in the cache. If cannot
 * 				  find one, register this.
 */
static bool
cache_lookup(void)
{
	cache_node_t	*node;
	hash_t			 hash = hash_proc_t(proc);
	
	/* If this is not our first call, lookup for the procedure. */
	for (node = cache_nodes; node; node = node->next)
		/* Compare procedures' OIDs, hash values and attributes. */
		if (node->proc->id == proc->id &&
			node->hash == hash &&
			cache_overlaps(node->proc))
		{ break; }

	/* Load found procedure and return. */
	if (node)
	{
		/*
		 * If it's present, must check whether it's still up to date. This is
		 * needed because CREATE OR REPLACE FUNCTION can modify the function's
		 * pg_proc entry without changing its OID.
		 */
		if (node->proc->xmin == proc->xmin &&
			node->proc->cmin == proc->cmin)
		{
			cache_load(node->proc);
			node->ncalls++;
			return true;
		}

		/* Remove the obsolete procedure from the caches. */
		cache_unregister(node);
	}

	return false;
}


/*
 * duplicate_arg_t - Duplicate given arg_t.
 */
static arg_t *
duplicate_arg_t(arg_t *args, int nargs)
{
	arg_t	*p;
	int		 i;

	if (!args || nargs < 1)
		return NULL;

	p = palloc(nargs * sizeof(arg_t));
	for (i = 0; i < nargs; i++)
	{
		
		/* Automatically copy inlined arg_t attributes. */
		memcpy((void *) &p[i], (void *) &args[i], sizeof(arg_t));

		p[i].name = pstrdup(args[i].name);
		
		if (args[i].isrow)
			p[i].args = duplicate_arg_t(args[i].args, args[i].nargs);
	}

	return p;
}


/*
 * crc32 - A simple CRC32 implementation.
 */
static int
crc32(unsigned char *buf, int len)
{
	int		i, j;
	int		hi = 0xFFFF, lo, flag;

	for ( i = 0; i < len; i++ ) {
		hi = hi ^ buf[i];

		for ( j = 1; j <= 8; j++ ) {
			flag = hi & 0x0001;
			hi = hi >> 1;
			if ( flag )
				hi = hi ^ 0xA001;
		}
	}
	
	lo = hi >> 8;
	hi = (hi << 8) | lo;
	hi &= 0xFFFF;

	return hi;
}


/*
 * hash_arg_t - Computes a hash value for an arg_t.
 */
static hash_t
hash_arg_t(arg_t *args, int nargs)
{
	int		i;
	hash_t	hash = hash_init();
	
	if (!args || nargs < 1)
		return hash;

	for (i = 0; i < nargs; i++)
	{
		char	buf[ARG_T_HASH_BUFFER_SIZE];
		int		l;

		if (args[i].isrow)
		{
			l = snprintf(buf, ARG_T_HASH_BUFFER_SIZE,
						 "%d%" ARG_T_HASH_NAME_SIZE "s%d%d%d%d",
						 args[i].isrow, args[i].name, args[i].type,
						 args[i].mode, args[i].isnull, args[i].nargs);
				
			hash_append(hash, hash_buffer(buf, l));
			hash_append(hash, hash_arg_t(args[i].args, args[i].nargs));
		}
		else
		{
			l = snprintf(buf, ARG_T_HASH_BUFFER_SIZE,
						 "%d%" ARG_T_HASH_NAME_SIZE "s%d%d%d%s",
						 args[i].isrow, args[i].name, args[i].type,
						 args[i].mode, args[i].isnull, args[i].value);
			hash_append(hash, hash_buffer(buf, l));
		}
	}

	return hash;
}


/*
 * hash_arg_t - Computes a hash value for a proc_t.
 */
static hash_t
hash_proc_t(proc_t *p)
{
	hash_t	hash = hash_init();
	char	buf[PROC_T_HASH_BUFFER_SIZE];
	int		l;

	/*
	 * Hash a trigger with just its necessary identifier attributes.
	 */
	if (p->istrigger)
	{
		hash_t	hash_args = hash_init();
		
		l = snprintf(buf, PROC_T_HASH_BUFFER_SIZE, "1%d%d%d",
					 p->tg_when, p->tg_event, p->tg_for);
		hash_append(hash, hash_buffer(buf, l));

		switch (p->tg_event)
		{
			case TG_ON_INSERT:
				hash_append(hash_args,
							hash_arg_t(p->tg_newtup, p->tg_newtup_natts));
				break;

			case TG_ON_DELETE:
				hash_append(hash_args,
							hash_arg_t(p->tg_oldtup, p->tg_oldtup_natts));
				break;

			case TG_ON_UPDATE:
				hash_append(hash_args,
							hash_arg_t(p->tg_newtup, p->tg_newtup_natts));
				hash_append(hash_args,
							hash_arg_t(p->tg_oldtup, p->tg_oldtup_natts));
				break;
				
		}
		
		hash_append(hash, hash_args);
	}

	/*
	 * Normal procedure hashing.
	 */
	else
		hash_append(hash, hash_arg_t(p->args, p->nargs));

	return hash;
}


/*
 * cache_unregister_deprecated - If cache size reaches maximum limit, traverse
 * 								 through cache nodes and remove the least
 * 								 called one.
 */
static void
cache_unregister_deprecated(void)
{
	cache_node_t	*n, *p;
	bool			 first = true;
	int				 min;

	if (cache_size < cache_max_size)
		return;

	/* Find least used node. */
	for (n = cache_nodes; n; n = n->next)
	{
		if (first)
		{
			p = n;
			min = n->ncalls;
			first = false;
		}
		else if (n->ncalls < min)
		{
			p = n;
			min = n->ncalls;
		}
	}

	/* Sorry child, somebody needs to be the victim. */
	cache_unregister(p);
}


/*
 * Below cache registration routines issues implicit memory context switches
 * during execution. We can graph the basic allocation concept in this form:
 *
 * cache_register_meta:
 *   current_mcxt = MemoryContextSwitchTo(TopTransactionContext);
 *   node = palloc(...);
 *   node->mcxt = AllocSetContextCreate(TopTransactionContext, ...);
 *   MemoryContextSwitchTo(node->mcxt);
 *   ...
 *
 * cache_register_res:
 *   MemoryContextSwitchTo(current_mcxt);
 */

/*
 * cache_register_meta - Register procedure's meta information in the cached
 * 						 node. You'll need to call cache_register_res() later to
 * 						 finish registration (by caching result data too).
 */
static void
cache_register_meta(void)
{
	cache_node_t	*n;
	proc_t			*p;
	char			 s[128];

	/* Make available room for addition. */
	cache_unregister_deprecated();

	/*
	 * cache_node_t allocation should be issued in the transaction's memory
	 * context. (We'll return to our current_mcxt later in the
	 * cache_register_res().)
	 */
	current_mcxt = MemoryContextSwitchTo(TopTransactionContext);
	n = palloc(sizeof(cache_node_t));
	n->ncalls = 0;

	/* Compute hash value of the current proc_t. */
	n->hash = hash_proc_t(proc);

	/* We'll need a fancy memory context name. */
	snprintf(s, 128, "plscheme-cached-proc-%d", proc->id);

	/*
	 * Create a new memory context for the to be cached procedure. This allows
	 * us to reclaim the function's storage cleanly.
	 */
	n->mcxt = AllocSetContextCreate(TopTransactionContext, s,
									ALLOCSET_DEFAULT_MINSIZE,
									ALLOCSET_DEFAULT_INITSIZE,
									ALLOCSET_DEFAULT_MAXSIZE);

	/* Switch to procedure's memory context. */
	MemoryContextSwitchTo(n->mcxt);
	
	p = n->proc = palloc(sizeof(proc_t));

	/*
	 * List of proc_t members will be needed to load procedure from the cache:
	 * 
	 *   Common members         : id, istrigger, resisnull, res, xmin, cmin.
	 *   For a basic procedure  : nargs, args.
	 *   For a trigger procedure: tg_when, tg_event, tg_for, tg_newtup_natts,
	 *   						  tg_oldtup_natts, tg_newtup, tg_oldtup,
	 *   						  tg_nargs, tg_args.
	 */
	
	p->id = proc->id;
	p->istrigger = proc->istrigger;
	p->xmin = proc->xmin;
	p->cmin = proc->cmin;
	
	/*
	 * resisnull and res attributes will be handled later by
	 * cache_register_res().
	 */

	/*
	 * Trigger procedure.
	 */
	if (proc->istrigger)
	{
		int	i;
		
		p->tg_when = proc->tg_when;
		p->tg_event = proc->tg_event;
		p->tg_for =proc->tg_for;

		switch (p->tg_event)
		{
			case TG_ON_INSERT:
				p->tg_newtup_natts = proc->tg_newtup_natts;
				p->tg_newtup = duplicate_arg_t(proc->tg_newtup,
											   proc->tg_newtup_natts);
				break;
				
			case TG_ON_DELETE:
				p->tg_oldtup_natts = proc->tg_oldtup_natts;
				p->tg_oldtup = duplicate_arg_t(proc->tg_oldtup,
											   proc->tg_oldtup_natts);
				break;
				
			case TG_ON_UPDATE:
				p->tg_newtup_natts = proc->tg_newtup_natts;
				p->tg_newtup = duplicate_arg_t(proc->tg_newtup,
											   proc->tg_newtup_natts);
				p->tg_oldtup_natts = proc->tg_oldtup_natts;
				p->tg_oldtup = duplicate_arg_t(proc->tg_oldtup,
											   proc->tg_oldtup_natts);
				break;
		}

		p->tg_nargs = proc->tg_nargs;
		p->tg_args = palloc(p->tg_nargs * sizeof(char *));
		for (i = 0; i < p->tg_nargs; i++)
			p->tg_args[i] = pstrdup(proc->tg_args[i]);
	}

	/*
	 * Basic procedure.
	 */
	else
	{
		p->nargs = proc->nargs;
		p->args = duplicate_arg_t(proc->args, proc->nargs);
	}
	
	/* Prepend cached procedure node to the list. */
	n->next = cache_nodes;
	cache_nodes = n;

	/* Let cache_register_res() to know our current getting cached node. */
	getting_cached_node = n;
}


/*
 * cache_register_res - Fill result attributes in the cached procedure node.
 * 						This will finish the registration process.
 */
static void
cache_register_res(void)
{
	cache_node_t	*node = getting_cached_node;

	/*
	 * Datum creation functions should have been called in the procedure's
	 * memory context (node->mcxt), therefore it's safe to just assign suitable
	 * links to the appropriate attributes in the cached cache_node_t.
	 */
	node->proc->resisnull = proc->resisnull;
	node->proc->res = proc->res;

	/* Increment cache_size. */
	cache_size++;

	/*
	 * We switched to TopMemoryContext in the cache_register_meta(). Now we need
	 * to return back to the procedure's memory (current_mcxt) context we were
	 * before.
	 */
	MemoryContextSwitchTo(current_mcxt);
}


/*
 * cache_free - Free allocated memory within specified cache_node_t.
 */
static void
cache_free(cache_node_t *node)
{
	/* Release proc_t storage. */
	MemoryContextDelete(node->mcxt);

	/* Release cache_node_t. (pfree() won't need a context switch.) */
	pfree(node);
}


/*
 * cache_unregister - Unregister specified node from the cache.
 */
static void
cache_unregister(cache_node_t *node)
{
	cache_node_t	*p;
	bool			 found = false;

	/* Neither cache_node, nor node can be null in this situation. */
	Assert(cache_nodes && node);

	/* Is target located in the root node? */
	if (cache_nodes == node)
	{
		cache_nodes = cache_nodes->next;
		goto ProceedRemove;
	}

	/* Sequentially traverse through nodes. */
	for (p = cache_nodes; p->next; p = p->next)
		if (p->next == node)
		{
			p->next = node->next;
			goto ProceedRemove;
		}

	/* Nor this should happen. */
	Assert(found);
	
ProceedRemove:
	/* Remove node. */
	cache_free(node);

	/* Decrement cache size. */
	cache_size--;
}


/*
 * build_arg_t - Builds an arg_t with respect to specified arguments.
 * 				 (Modes will be treated as PROARGMODE_IN if not specified.)
 */
static arg_t *
build_arg_t(int nargs, Oid *types, char **names, char *modes,
			bool *nulls, Datum *values)
{
	arg_t	*args = palloc(nargs * sizeof(arg_t));
	int		 i;

	for (i = 0; i < nargs; i++)
	{
		HeapTuple		typetup;
		Form_pg_type	typestruct;
		bool			ispseudorecord;

		args[i].type = types[i];
		args[i].name = names[i];
		args[i].mode = (modes) ? modes[i] : PROARGMODE_IN;

		/*
		 * Despite isnull flag is meaningless for OUT arguments, we
		 * set it true for OUT arguments to avoid from redundant
		 * processing. (See !args[i].isnull checks below.)
		 */
		args[i].isnull = (args[i].mode != PROARGMODE_OUT) ? nulls[i] : true;
		
		typetup = SearchSysCache(TYPEOID, ObjectIdGetDatum(types[i]), 0, 0, 0);
		if (!HeapTupleIsValid(typetup))
			elog(ERROR, "Cache lookup failed for type %u.", types[i]);
		typestruct = (Form_pg_type) GETSTRUCT(typetup);
		ispseudorecord = (typestruct->typtype == 'p' &&
						  types[i] == RECORDOID);

		/* 
		 * Basic and domain types.
		 */
		if (typestruct->typtype == 'b' ||
			typestruct->typtype == 'd')
		{
			args[i].isrow = false;

			args[i].typename = NameStr(typestruct->typname);
			args[i].typemod = typestruct->typtypmod;
			args[i].typeioparam = getTypeIOParam(typetup);
			args[i].inputfn = typestruct->typinput;

			if (!args[i].isnull)
				args[i].value = OidOutputFunctionCall(typestruct->typoutput,
													  values[i]);
		}
		
		/*
		 * Complex (ie. a table's rowtype) or pseudo (record) type.
		 */
		else if (typestruct->typtype == 'c' || ispseudorecord)
		{
			HeapTupleHeader	  tuph;
			Oid				  tuptype;
			int32			  tuptypmod;
			TupleDesc		  tupdesc;
			HeapTupleData	  tupdata;
			int				  j, k;
			int				  sub_nargs;
			Oid				 *sub_types;
			char			**sub_names;
			bool			 *sub_nulls;
			Datum			 *sub_values;
			
			args[i].isrow = true;

			/* Skip redundant processing for NULL and OUT arguments. */
			if (args[i].isnull)
				goto EndOfLoop;

			tuph = DatumGetHeapTupleHeader(values[i]);
			
			/* Extract rowtype info and find a tupdesc. */
			tuptype = HeapTupleHeaderGetTypeId(tuph);
			tuptypmod = HeapTupleHeaderGetTypMod(tuph);
			tupdesc = lookup_rowtype_tupdesc(tuptype, tuptypmod);
	
			/* Build a temporary HeapTuple control structure. */
			tupdata.t_len = HeapTupleHeaderGetDatumLength(tuph);
			tupdata.t_data = tuph;

			/* Calculate will be required room count. */
			for (j = sub_nargs = 0; j < tupdesc->natts; j++)
				if (!tupdesc->attrs[j]->attisdropped || ispseudorecord)
					sub_nargs++;

			sub_types = palloc(sub_nargs * sizeof(Oid));
			sub_names = palloc(sub_nargs * sizeof(char *));
			sub_nulls = palloc(sub_nargs * sizeof(bool));
			sub_values = palloc(sub_nargs * sizeof(Datum));
			
			for (j = k = 0; j < tupdesc->natts; j++)
				/*
				 * Record row types are logically invisible, therefore their
				 * attisdropped attribute is always true.
				 */
				if (!tupdesc->attrs[j]->attisdropped || ispseudorecord)
				{
					sub_values[k] = heap_getattr(&tupdata, (j + 1), tupdesc,
												 &sub_nulls[k]);
					sub_types[k] = tupdesc->attrs[j]->atttypid;
					sub_names[k] = NameStr(tupdesc->attrs[j]->attname);

					k++;
				}

			/* Recursively parse inner tuple. */
			args[i].nargs = sub_nargs;
			args[i].args = build_arg_t(sub_nargs, sub_types, sub_names,
									   NULL, sub_nulls, sub_values);
			
			ReleaseTupleDesc(tupdesc);
		}
		
		/*
		 * Pseudo types. (Except record, handled above.)
		 */
		else
			ereport(ERROR,
					(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
					 errmsg("PL/scheme functions cannot handle %s type.",
						    NameStr(typestruct->typname))));

EndOfLoop:
		ReleaseSysCache(typetup);
	}

	return args;
}


/*
 * arg_t_from_tuple - Same as above; just takes a HeapTuple and a
 * 					  TupleDesc as argument.
 */
static arg_t *
arg_t_from_tuple(HeapTuple tup, TupleDesc tupdesc, int *in_nargs)
{
	int		  i, j;
	int		  nargs;
	Oid		 *types;
	char	**names;
	bool	 *nulls;
	Datum	 *values;

	/* Calculate available attributes. */
	for (i = nargs = 0; i < tupdesc->natts; i++)
		if (!tupdesc->attrs[i]->attisdropped)
			nargs++;

	types = palloc(nargs * sizeof(Oid));
	names = palloc(nargs * sizeof(char *));
	nulls = palloc(nargs * sizeof(bool));
	values = palloc(nargs * sizeof(Datum));

	for (i = j = 0; i < tupdesc->natts; i++)
		if (!tupdesc->attrs[i]->attisdropped)
		{
			values[j] = heap_getattr(tup, (i + 1), tupdesc, &nulls[j]);
			names[j] = NameStr(tupdesc->attrs[i]->attname);
			types[j] = tupdesc->attrs[i]->atttypid;

			++j;
		}
	
	*in_nargs = nargs;
	return build_arg_t(nargs, types, names, NULL, nulls, values);
}


/*
 * parse_func_args - Parse procedure's IN/OUT arguments and place them into
 *					 proc struct with their values.
 */
static void
parse_func_args(FunctionCallInfo fcinfo, HeapTuple proctup,
				Form_pg_proc procstruct)
{
	Oid		 *types;
	char	**names;
	char	 *modes;
	int		  i, j;
	
	/* Gather arguments' name, type, mode information. */
	proc->nargs = get_func_arg_info(proctup, &types, &names, &modes);
	
	/* I'm quite declined to use $1, $2, etc. kind of parameters. */
	if (proc->nargs > 0 && !names)
		ereport(ERROR,
				(errcode(ERRCODE_SYNTAX_ERROR),
				 errmsg("You must supply argument aliases in "
					 	"PL/scheme procedures.")));

	/* Disallow duplicate usage of the same name. */
	for (i = 0; i < proc->nargs; i++)
		for (j = (i + 1); j < proc->nargs; j++)
			if (!strcmp(names[i], names[j]))
				ereport(ERROR,
						(errcode(ERRCODE_SYNTAX_ERROR),
						 errmsg("Found duplicated argument aliases at "
							 	"positions %d and %d.", i, j)));

	/* Build arg_t array. */
	proc->args = build_arg_t(proc->nargs, types, names, modes,
							 fcinfo->argnull, fcinfo->arg);
}


/*
 * parse_trig_args - Parse trigger arguments. (Trigger name, level,
 * 					 operation, etc.)
 */
static void
parse_trig_args(FunctionCallInfo fcinfo)
{
	TriggerData	*tgdata = (TriggerData *) fcinfo->context;
	TupleDesc	 tgtupdesc = tgdata->tg_relation->rd_att;
	HeapTuple	 tgtup = tgdata->tg_trigtuple;
	HeapTuple	 tgnewtup = tgdata->tg_newtuple;

	/* Miscellaneous trigger arguments. */
	proc->tg_relid = tgdata->tg_relation->rd_id;
	proc->tg_name = tgdata->tg_trigger->tgname;
	proc->tg_relname = SPI_getrelname(tgdata->tg_relation);
	proc->tg_tablename = SPI_getrelname(tgdata->tg_relation);
	proc->tg_tableschema = SPI_getnspname(tgdata->tg_relation);

	/* When does this get triggered? */
	if (TRIGGER_FIRED_BEFORE(tgdata->tg_event))
		proc->tg_when = TG_BEFORE;
	else if (TRIGGER_FIRED_AFTER(tgdata->tg_event))
		proc->tg_when = TG_AFTER;
	else
		elog(ERROR, "Unrecognized trigger event!");

	/* Trigged for what? */
	if (TRIGGER_FIRED_FOR_ROW(tgdata->tg_event))
		proc->tg_for = TG_FOR_ROW;
	else if (TRIGGER_FIRED_FOR_STATEMENT(tgdata->tg_event))
		proc->tg_for = TG_FOR_STMT;
	else
		elog(ERROR, "Unrecognized trigger level!");

	/* Trigged on what? */
	if (TRIGGER_FIRED_BY_INSERT(tgdata->tg_event))
	{
		proc->tg_event = TG_ON_INSERT;
		if (proc->tg_for == TG_FOR_ROW)
			proc->tg_newtup = arg_t_from_tuple(tgtup, tgtupdesc,
											   &proc->tg_newtup_natts);
	}
	else if (TRIGGER_FIRED_BY_DELETE(tgdata->tg_event))
	{
		proc->tg_event = TG_ON_DELETE;
		if (proc->tg_for == TG_FOR_ROW)
			proc->tg_oldtup = arg_t_from_tuple(tgtup, tgtupdesc,
											   &proc->tg_oldtup_natts);
	}
	else if (TRIGGER_FIRED_BY_UPDATE(tgdata->tg_event))
	{
		proc->tg_event = TG_ON_UPDATE;
		if (proc->tg_for == TG_FOR_ROW)
		{
			proc->tg_oldtup = arg_t_from_tuple(tgtup, tgtupdesc,
											   &proc->tg_oldtup_natts);
			proc->tg_newtup = arg_t_from_tuple(tgnewtup, tgtupdesc,
											   &proc->tg_newtup_natts);
		}
	}
	else
		elog(ERROR, "Unrecognized trigger operation!");

	/* Arguments passed to function. */
	proc->tg_nargs = tgdata->tg_trigger->tgnargs;
	proc->tg_args = tgdata->tg_trigger->tgargs;
}


/*
 * convert_from_errcode - Convert an SQL error code into its corresponding
 *						  SCM value from the exception table.
 */
static SCM
convert_from_errcode(int sqlerrcode)
{
	int		l = sizeof(excptbl) / sizeof(excptbl[0]);
	int		i;
	bool	found = -1;

	for (i = 0; i < l; i++)
		if (sqlerrcode == excptbl[i].code)
		{
			found = i;
			break;
		}

	/* This shouldn't happen! */
	Assert(i != -1);
	
	return excptbl[i].sym;
}


/*
 * alist_from_arg_t - Builds an associative list from given arg_t.
 */
static SCM
alist_from_arg_t(arg_t *args, int nargs, char discard_mode, bool inner)
{
	SCM	alist = SCM_EOL;
	int	i;

	for (i = 0; i < nargs; i++)
	{
		SCM	val;
		
		/* Discard arguments of unwanted mode. */
		if (args[i].mode == discard_mode)
			continue;

		/* Don't bother with below. This is just a dull NULL. */
		if (args[i].isnull)
			val = SCM_EOL;

		/* Hrm... Looks like this will need an inner alist. */
		else if (args[i].isrow)
			val = alist_from_arg_t(args[i].args, args[i].nargs,
								   discard_mode, true);

		/* Hey! That's a basic type. */
		else
			/* Assign result returned from data imposition. */
			val = SCM_DATA_IMPOSE(args[i].value, args[i].typename);

		/*
		 * We need to cons outer attributes' label as symbols, because we'll
		 * pass them to scm_define().
		 */
		alist = scm_acons((inner
						   ? scm_from_locale_string(args[i].name)
						   : scm_from_locale_symbol(args[i].name)), val, alist);
	}

	return alist;
}


/*
 * place_func_args - Place parsed function arguments into Scheme code.
 */
static SCM
place_func_args(SCM data)
{
	SCM	alist = alist_from_arg_t(proc->args, proc->nargs,
								 PROARGMODE_OUT, false);

	/* Validating guile_execution_module before argument definitions. */
	Assert(guile_execution_module);

	while (!scm_is_null(alist))
	{
		SCM	key = scm_caar(alist);
		SCM	val = scm_cdar(alist);

		scm_define(key, val);
		alist = scm_cdr(alist);
	}

	return SCM_BOOL_T;
}


/*
 * place_trig_args - Place parsed trigger arguments into Guile.
 */
static SCM
place_trig_args(SCM data)
{
	static bool	trig_syms_placed = false;
	
	/*
	 * Place trigger symbols. (Just once for every session.)
	 */
	if (!trig_syms_placed)
	{
#define MAKE_TG_SYM(indice, sym) \
		tgsym[indice] = scm_from_locale_symbol(sym)

		MAKE_TG_SYM(TG_BEFORE,		"tg-before");
		MAKE_TG_SYM(TG_AFTER,		"tg-after");
		MAKE_TG_SYM(TG_ON_INSERT,	"tg-on-insert");
		MAKE_TG_SYM(TG_ON_DELETE,	"tg-on-delete");
		MAKE_TG_SYM(TG_ON_UPDATE,	"tg-on-update");
		MAKE_TG_SYM(TG_FOR_ROW,		"tg-for-row");
		MAKE_TG_SYM(TG_FOR_STMT,	"tg-for-stmt");

		trig_syms_placed = true;
	}

	/* Validate execution module. */
	Assert(guile_execution_module);
	
	/*
	 * Place miscellaneous trigger arguments.
	 */
	scm_c_define("tg-relid",		scm_from_int(proc->tg_relid));
	scm_c_define("tg-name",			scm_from_locale_string(proc->tg_name));
	scm_c_define("tg-relname",		scm_from_locale_string(proc->tg_relname));
	scm_c_define("tg-tablename",	scm_from_locale_string(proc->tg_tablename));
	scm_c_define("tg-tableschema",	scm_from_locale_string(proc->tg_tableschema));
	scm_c_define("tg-when",			tgsym[proc->tg_when]);
	scm_c_define("tg-event",		tgsym[proc->tg_event]);
	scm_c_define("tg-for",			tgsym[proc->tg_for]);

	/*
	 * Place NEW/OLD tuples.
	 */
	if (proc->tg_for == TG_FOR_ROW)
		switch (proc->tg_event)
		{
			case TG_ON_INSERT:
				scm_c_define("tg-tuple-new",
							 alist_from_arg_t(proc->tg_newtup,
								 			  proc->tg_newtup_natts,
											  PROARGMODE_OUT, true));
				break;

			case TG_ON_DELETE:
				scm_c_define("tg-tuple-old",
							 alist_from_arg_t(proc->tg_oldtup,
								 			  proc->tg_oldtup_natts,
											  PROARGMODE_OUT, true));
				break;

			case TG_ON_UPDATE:
				scm_c_define("tg-tuple-new",
							 alist_from_arg_t(proc->tg_newtup,
								 			  proc->tg_newtup_natts,
											  PROARGMODE_OUT, true));
				scm_c_define("tg-tuple-old",
							 alist_from_arg_t(proc->tg_oldtup,
								 			  proc->tg_oldtup_natts,
											  PROARGMODE_OUT, true));
				break;
		}

	return SCM_BOOL_T;
}


/*
 * check_tuple_consistency - Checks whether all tuple attributes are satisfied
 *							 or not by looking at given arg_t array and tuple_t.
 *							 (Attribute order in the tuple returned from Scheme
 *							 code (if possible) will be re-ordered in here with
 *							 respect to given arg_t.)
 */
static void
check_tuple_consistency(tuple_t *tuple, arg_t *args, int nargs)
{
	int	i;

	/* If tuple is NULL, there is no need for further checking. */
	if (tuple->isnull)
		return;
	
	/* First make a quick array length control. */
	if (tuple->natts != nargs)
		ereport(ERROR,
				(errcode(ERRCODE_DATATYPE_MISMATCH),
				 errmsg("Required argument count (%d) is not "
					 	"satisfied. (Found %d attributes.)",
						nargs, tuple->natts)));

	/*
	 * Check tuple attributes one by one.
	 */
	for (i = 0; i < nargs; i++)
	{
		/*
		 * Compare attribute names.
		 */
		if (strcmp(tuple->names[i], args[i].name))
		{
			int	j;
			int	found = -1;

			/* Did user really supply an attribute with that name? */
			for (j = i; j < nargs; j++)
				if (strcmp(tuple->names[j], args[i].name) == 0)
				{
					found = j;
					break;
				}

			/*
			 * Swap found attribute with the current one.
			 */
			if (found > -1)
			{
				char	*tmp_str;
				Oid		 tmp_oid;
				char	 tmp_char;
				bool	 tmp_bool;
				Datum	 tmp_datum;

				tmp_str = tuple->names[i];
				tuple->names[i] = tuple->names[j];
				tuple->names[j] = tmp_str;

				tmp_oid = tuple->types[i];
				tuple->types[i] = tuple->types[j];
				tuple->types[j] = tmp_oid;

				tmp_char = tuple->modes[i];
				tuple->modes[i] = tuple->modes[j];
				tuple->modes[j] = tmp_char;

				tmp_bool = tuple->nulls[i];
				tuple->nulls[i] = tuple->nulls[j];
				tuple->nulls[j] = tmp_bool;

				tmp_datum = tuple->values[i];
				tuple->values[i] = tuple->values[j];
				tuple->values[j] = tmp_datum;
			}
			else
				ereport(ERROR,
						(errcode(ERRCODE_DATATYPE_MISMATCH),
						 errmsg("Expecting an attribute with name `%s' "
							 	"(instead of `%s') at position %d.",
								args[i].name, tuple->names[i], i)));
		}
		
		/* Compare attribute modes. */
		if (!(tuple->modes[i] == args[i].mode ||
			  args[i].mode == PROARGMODE_INOUT))
			ereport(ERROR,
					(errcode(ERRCODE_DATATYPE_MISMATCH),
					 errmsg("Expecting mode `%c' (instead of `%c') "
						 	"at position %d.",
							args[i].mode, tuple->modes[i], i)));

		/* No need to bother with type checking if attribute value is NULL. */
		if (tuple->nulls[i])
			/*
			 * Make suitable casting. (Actually, when an attribute is NULL,
			 * there's no need for any extra type checking. But type OID can be
			 * used later when building a TupleDesc.)
			 */
			tuple->types[i] = args[i].type;

		/* Compare attribute types. */
		if (tuple->types[i] != args[i].type)
			ereport(ERROR,
					(errcode(ERRCODE_DATATYPE_MISMATCH),
					 errmsg("Expecting an attribute with type `%s' "
						 	"(instead of `%s') at position %d.",
							format_type_be(args[i].type),
							format_type_be(tuple->types[i]), i)));

		/*
		 * We don't need to make any further comparisons in here, like checking
		 * inner attributes of a record type attribute. Because any attribute
		 * nested within a record type is not determinable.
		 */
	}
}


/*
 * build_tupledesc - Build a new TupleDesc with respect to specified
 * 					 attribute types, modes and names.
 */
static TupleDesc
build_tupledesc(int nelems, Oid *types, char *modes, char **names)
{
	TupleDesc	tupdesc;
	int		 	i;

	/* Validate passed array length. */
	Assert(nelems > 0);

	/* Create template. */
	tupdesc = CreateTemplateTupleDesc(nelems, false);

	/* Place attributes' meta-information. */
	for (i = 0; i < nelems; i++)
		TupleDescInitEntry(tupdesc, (i + 1), names[i], types[i], -1, 0);

	/* Register record type. */
	tupdesc = BlessTupleDesc(tupdesc);

	return tupdesc;
}


/*
 * heap_from_tuple_t - Forms HeapTuple (and TupleDesc) from specified tuple_t.
 */
static HeapTuple
heap_from_tuple_t(tuple_t *tuple, TupleDesc tupdesc)
{
	/* Caller must handle NULL previously. */
	Assert(!tuple->isnull);

	tupdesc = build_tupledesc(tuple->natts, tuple->types,
							  tuple->modes, tuple->names);
	
	return heap_form_tuple(tupdesc, tuple->values, tuple->nulls);
}


/*
 * tuple_t_from_alist - Builds a tuple_t from specified association list.
 */
static tuple_t *
tuple_t_from_alist(SCM alist, char mode, bool inner, arg_t *args, int nargs)
{
	tuple_t	*tuple = palloc(sizeof(tuple_t));
	SCM		 cdr;
	int		 i;

	/* Handle NULL case first. */
	if (scm_is_null(alist))
	{
		tuple->isnull = true;
		return tuple;
	}
	tuple->isnull = false;
	
	/* Check whether we received an associative list or not. */
	if (!scm_is_list(alist))
		ereport(ERROR,
				(errcode(ERRCODE_DATATYPE_MISMATCH),
				 errmsg("You must return an association list for a "
					 	"record type.")));

	/* Calculate will be required room count. */
	for (i = 0, cdr = alist; !scm_is_null(cdr); cdr = scm_cdr(cdr), i++)
		;

	tuple->natts = i;
	tuple->names = palloc(tuple->natts * sizeof(char *));
	tuple->types = palloc(tuple->natts * sizeof(Oid));
	tuple->values = palloc(tuple->natts * sizeof(Datum));
	tuple->nulls = palloc(tuple->natts * sizeof(bool));

	/* Parse alist elements and fill attribute values. */
	for (i = 0, cdr = alist; !scm_is_null(cdr); cdr = scm_cdr(cdr), i++)
	{
		SCM	key = scm_caar(cdr);
		SCM	val = scm_cdar(cdr);

		SCM_SAFE_STRDUP(key, tuple->names[i]);

		/* Reset NULL flag. */
		tuple->nulls[i] = false;

		/* Handle NULL. */
		if (scm_is_null(val))
		{
			tuple->values[i] = (Datum) 0;
			tuple->nulls[i] = true;
		}

		/*
		 * Check if this tuple holds a nested tuple.
		 */
		else if (scm_is_list(val))
		{
			HeapTuple	 tup;
			TupleDesc	 tupdesc;
			tuple_t		*subtuple;

			subtuple = tuple_t_from_alist(val, mode, true, args, nargs);
			tup = heap_from_tuple_t(subtuple, tupdesc);

			tuple->values[i] = HeapTupleGetDatum(tup);
			tuple->types[i] = RECORDOID;
		}

		/*
		 * Single attribute.
		 */
		else
		{
			SCM		 ret;
			SCM		 scm_value;
			SCM		 scm_typename;
			char	*value;
			char	*typename;
			Oid		 typid;
			regproc	 typinput;
			Oid		 typioparam;
			int		 typmod;
			
			/* Apply data exposition filters. */
			ret = SCM_DATA_EXPOSE(val);
			
			scm_value = scm_car(ret);
			SCM_SAFE_STRDUP(scm_value, value);
			
			scm_typename = scm_cadr(ret);
			SCM_SAFE_STRDUP(scm_typename, typename);
			
			/*
			 * If this is an inner tuple, we'll use type returned from
			 * pl-type-expose.
			 */
			if (inner)
			{
				HeapTuple		typetup;
				Form_pg_type	typestruct;

				parseTypeString(typename, &typid, &typmod);

				typetup = SearchSysCache(TYPEOID,
										 ObjectIdGetDatum(typid),
										 0, 0, 0);
				if (!HeapTupleIsValid(typetup))
					elog(ERROR, "Cache lookup failed for type (%d) %s.",
						 typid, typename);
				typestruct = (Form_pg_type) GETSTRUCT(typetup);
				typinput = typestruct->typinput;
				typioparam = getTypeIOParam(typetup);
				ReleaseSysCache(typetup);
			}

			/*
			 * This is not an attribute nested within a record type. Therefore,
			 * it must exist in the passed procedure arg_t. Locate and use
			 * type information supplied by the related arg_t item.
			 */
			else
			{
				int		j;
				bool	found = false;

				/*
				 * We guranteed the uniqueness of arguments in
				 * parse_func_args() phase. Therefore, there is no harm to use
				 * the first argument that matches.
				 */
				for (j = 0; j < nargs; j++)
					if (!strcmp(args[j].name, tuple->names[i]))
					{
						found = true;
						break;
					}

				if (!found)
					ereport(ERROR,
							(errcode(ERRCODE_SYNTAX_ERROR),
							 errmsg("Cannot find an argument with name %s!",
								 	tuple->names[i])));

				typid = args[j].type;
				typinput = args[j].inputfn;
				typmod = args[j].typemod;
				typioparam = args[j].typeioparam;
			}
			
			tuple->types[i] = typid;
			tuple->values[i] = OidInputFunctionCall(typinput, value,
													typioparam, typmod);
		}
	}

	/* Place attribute modes. */
	tuple->modes = palloc(tuple->natts * sizeof(char));
	memset(tuple->modes, mode, (tuple->natts * sizeof(char)));

	return tuple;
}


/*
 * extract_wrt_mode - Extracts sufficient members from given arg_t array with
 *					  respect to specified argument mode. Arguments with the
 *					  specified mode will be discarded.
 */
static arg_t *
extract_wrt_mode(arg_t *in_args, int in_nargs, char mode, int *nargs)
{
	int		 i, j;
	arg_t	*args;

	/* Calculate will be required room count. */
	for (i = j = 0; i < in_nargs; i++)
		if (in_args[i].mode != mode)
			j++;
	
	args = palloc(j * sizeof(arg_t));
	*nargs = j;
	
	/* Copy arguments of specified mode. */
	for (i = j = 0; i < in_nargs; i++)
		if (in_args[i].mode != mode)
			memcpy((void *) &args[j++], (void *) &in_args[i],
				   sizeof(arg_t));

	return args;
}


/*
 * datum_from_func_res - Accessor function for handle_func_res(). Parses given
 * 						 SCM and transforms it into a suitable Datum. If nargs,
 * 						 args or tupdesc is NULL, they will be assigned from
 * 						 scratch. (That is for providing some kind of caching
 * 						 to avoid from computing same values over and over
 * 						 again.)
 */
static Datum
datum_from_func_res(SCM res, bool *resisnull, int *nargs, arg_t *args,
					TupleDesc tupdesc)
{
	Datum	ret;

	/* Reset isnull flag. */
	*resisnull = false;
	
	/*
	 * Handle NULL case.
	 */
	if (scm_is_null(res))
	{
		*resisnull = true;
		ret = (Datum) 0;
	}

	/*
	 * We'll return a single basic type.
	 */
	else if (!proc->resisrow)
	{
		SCM				 scm_value;
		char			*value;
		HeapTuple		 typetup;
		Form_pg_type	 typestruct;
		regproc			 typinput;
		int				 typmod;
		Oid				 typioparam;
		
		/* Apply data exposition filters. */
		res = SCM_DATA_EXPOSE(res);

		scm_value = scm_car(res);
		SCM_SAFE_STRDUP(scm_value, value);

		/* Get typinput, typmod, typioparam properties of the type. */
		typetup = SearchSysCache(TYPEOID,
								 ObjectIdGetDatum(proc->restype),
								 0, 0, 0);
		if (!HeapTupleIsValid(typetup))
			elog(ERROR, "Cache lookup failed for type %d.", proc->restype);
		typestruct = (Form_pg_type) GETSTRUCT(typetup);
		typinput = typestruct->typinput;
		typmod = typestruct->typtypmod;
		typioparam = getTypeIOParam(typetup);
		ReleaseSysCache(typetup);

		ret = OidInputFunctionCall(typinput, value, typioparam, typmod);
	}

	/*
	 * Return a row type (record).
	 *
	 * Currently, the only way to return a record type in PL/scheme procedures
	 * is via using INOUT/OUT argument modes. Scheme code must return an
	 * associative list with (key . value) pairs and keys must overlap with the
	 * specified INOUT/OUT arguments. Programmer must follow the same INOUT/OUT
	 * parameters' name order in the returned associative list.
	 */
	else
	{
		tuple_t		*tuple;
		HeapTuple	 heaptup;
		
		if (!args)
		{
			/* Extract INOUT/OUT arguments. */
			args = extract_wrt_mode(proc->args, proc->nargs,
									PROARGMODE_IN, nargs);
			
			if (*nargs < 2)
				ereport(ERROR,
						(errcode(ERRCODE_DATATYPE_MISMATCH),
						 errmsg("You must specify at least two arguments in "
							 	"INOUT/OUT mode to return a record type.")));
		}

		/* Build tuple_t from returned SCM. */
		tuple = tuple_t_from_alist(res, PROARGMODE_OUT, false, args, *nargs);

		/* Check whether all tuple attributes are satisfied or not. */
		check_tuple_consistency(tuple, args, *nargs);

		if (!tupdesc)
		{
			/* Caller must handle NULL previously. */
			Assert(!tuple->isnull);
			
			tupdesc = build_tupledesc(tuple->natts, tuple->types,
									  tuple->modes, tuple->names);

			heaptup = heap_form_tuple(tupdesc, tuple->values, tuple->nulls);
		}
		else
			heaptup = heap_from_tuple_t(tuple, tupdesc);

		ret = HeapTupleGetDatum(heaptup);
	}

	return ret;
}


/*
 * handle_func_res - Handles result returned from a non-trigger
 * 					 procedure execution.
 */
static SCM
handle_func_res(void *data)
{
	SCM		 res = (SCM) data;
	int		 nargs;
	arg_t	*args;

	/*
	 * Handle return type of void.
	 */
	if (proc->restype == VOIDOID)
	{
		/* Treat as NULL. (Actually, PostgreSQL will just ignore the result.) */
		proc->resisnull = true;
		proc->res = (Datum) 0;
	}

	/*
	 * Return a non-set result.
	 */
	else if (!proc->resisset)
	{
		TupleDesc	tupdesc;
		
		/* Reset arguments and TupleDesc. */
		args = NULL;
		tupdesc = NULL;
		
		proc->res = datum_from_func_res(res, &proc->resisnull,
									   &nargs, args, tupdesc);
	}

	/*
	 * Return from a SRF.
	 */
	else
	{
		ReturnSetInfo	*rsi = proc->rs_info;
		MemoryContext	 oldcxt;
		MemoryContext	 rscxt;
		Tuplestorestate	*rsstate;
		SCM				 cdr;
		
		/*
		 * There're 2 ways to return from an SRF:
		 * 
		 * 1. Value-per-call Mode
		 *    You return each tuple one by one via SRF_RETURN_NEXT() macro. But
		 *    PG_RETURN_DATUM() calls in the macro, makes it quite
		 *    impracticble. OTOH, this method gives opportunity to call SRFs in
		 *    a fashion like "SELECT mysrf();"
		 *
		 * 2. Materialize Mode
		 *    In this mode, you collect all tuples in a single set and return
		 *    that set. When compared to previos method, it's not possible to
		 *    use SRF of materialize mode like "SELECT my_materialized_srf();",
		 *    instead, you need to access it as a simple table: "SELECT * FROM
		 *    my_materialized_srf();".
		 *
		 * In here we'll prefer materialize mode, because of the impractibility
		 * of value-per-call mode.
		 */

		/* Initialize tuple store. */
		rscxt = rsi->econtext->ecxt_per_query_memory;
		oldcxt = MemoryContextSwitchTo(rscxt);
		rsstate = tuplestore_begin_heap(true, false, work_mem);
		MemoryContextSwitchTo(oldcxt);

		/* Check caller can handle a set result in the way we want. */
		if (!rsi || !IsA(rsi, ReturnSetInfo) ||
			(rsi->allowedModes & SFRM_Materialize) == 0 ||
			rsi->expectedDesc == NULL)
		{
			ereport(ERROR,
					(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
					 errmsg("SRF called in context that cannot accept a set.")));
		}
		rsi->returnMode = SFRM_Materialize;

		/* Fix procedure result. */
		proc->res = (Datum) 0;
		
		/*
		 * Handle NULL result.
		 */
		if (scm_is_null(res))
		{
			proc->resisnull = true;
			return SCM_BOOL_T;
		}
			
		/* Check whether we received an associative list or not. */
		if (!scm_is_list(res))
			ereport(ERROR,
					(errcode(ERRCODE_DATATYPE_MISMATCH),
					 errmsg("You must return a list of tuples "
						 	"while returning from an SRF.")));

		for (cdr = res; !scm_is_null(cdr); cdr = scm_cdr(cdr))
		{
			SCM				 car = scm_car(cdr);
			tuple_t			*tuple;
			TupleDesc		 tupdesc = rsi->expectedDesc;
			HeapTuple		 heaptup;

			/* Extract arguments at the first call. */
			if (cdr == res)
			{
				/* Extract INOUT/OUT arguments. */
				args = extract_wrt_mode(proc->args, proc->nargs,
										PROARGMODE_IN, &nargs);
			
				if (nargs < 1)
					ereport(ERROR,
							(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
							 errmsg("You must specify at least one INOUT/OUT "
								 	"arguments in a SRF. This information "
									"will be used for checking returned "
									"tuples' attribute integrity.")));
			}
		
			/* Build tuple_t from returned SCM. */
			tuple = tuple_t_from_alist(car, PROARGMODE_OUT, false, args, nargs);
			if (tuple->isnull)
				ereport(ERROR,
						(errcode(ERRCODE_DATATYPE_MISMATCH),
						 errmsg("You cannot return NULL tuple in an SRF.")));

			/* Check whether all tuple attributes are satisfied or not. */
			check_tuple_consistency(tuple, args, nargs);

			/* Form HeapTuple. */
			heaptup = heap_form_tuple(tupdesc, tuple->values, tuple->nulls);
			
			/* Append created tuple to the set. */
			oldcxt = MemoryContextSwitchTo(rscxt);
			tuplestore_puttuple(rsstate, heaptup);
			MemoryContextSwitchTo(oldcxt);

			/*
			 * Free created tuple. (tuplestore_puttuple() has just
			 * gotten a copy of this.)
			 */
			heap_freetuple(heaptup);
		}

		/* Assign result set. */
		rsi->setResult = rsstate;

		/* We should have just formed the result wrt expected TupleDesc. */
		rsi->setDesc = rsi->expectedDesc;
	}
	
	return SCM_BOOL_T;
}


/*
 * handle_trig_res - Form an appropriate function return value from the
 * 					 returned SCM.
 */
static SCM
handle_trig_res(void *data)
{
	SCM	res = (SCM) data;

	/*
	 * In case of a
	 *   1. After,
	 *   2. Per-statement,
	 *   3. NULL returned
	 * trigger, we'll return NULL.
	 */
	if (proc->tg_when == TG_AFTER ||	/* [1] */
		proc->tg_for == TG_FOR_STMT ||	/* [2] */
		scm_is_null(res))				/* [3] */
	{
		/*
		 * Trigger protocol allows function to return a null pointer,
		 * but NOT to set the isnull result flag.
		 */
		proc->res = (Datum) 0;
		return SCM_BOOL_T;
	}

	/*
	 * Form a HeapTuple with required attributes from the returned
	 * associative list.
	 */
	else
	{
		TupleDesc	 tupdesc;
		tuple_t		*tuple;
		arg_t		*args;
		int			 nargs;

		/*
		 * TODO: It's possible to skip below steps if user returned exactly the
		 * same tg-tuple-new or tg-tuple-old we passed to it and return original
		 * HeapTuple of the related tuple.
		 */

		/*
		 * Which arg_t we ported to Guile?
		 */
		if (proc->tg_event == TG_ON_INSERT ||
			proc->tg_event == TG_ON_UPDATE)
		{
			args = proc->tg_newtup;
			nargs = proc->tg_newtup_natts;
		}
		else if (proc->tg_event == TG_ON_DELETE)
		{
			args = proc->tg_oldtup;
			nargs = proc->tg_oldtup_natts;
		}

		/* Build tuple_t from returned SCM. */
		tuple = tuple_t_from_alist(res, PROARGMODE_IN, false, args, nargs);

		/* Check whether all tuple attributes are satisfied or not. */
		check_tuple_consistency(tuple, args, nargs);

		/* Place result HeapTuple. */
		proc->resisnull = false;
		proc->res = (Datum) heap_from_tuple_t(tuple, tupdesc);
	}

	return SCM_BOOL_T;
}


/*
 * Soft-port related stuff to manage I/O emulation between Guile
 * and PL handler.
 */

static SCM
soft_port_handler_c(SCM in_c)
#define FUNC_NAME "internal soft_port_handler_c"
{
	char	*s;
	char	 c;
	
	SCM_VALIDATE_STRING(0, in_c);
	
	s = scm_to_locale_string(in_c);
	c = *s;
	free(s);
	appendStringInfoChar(soft_port_si, c);

	return SCM_BOOL_T;
}
#undef FUNC_NAME

static SCM
soft_port_handler_s(SCM in_s)
#define FUNC_NAME "internal soft_port_handler_s"
{
	int		 l;
	char	*p;

	SCM_VALIDATE_STRING(0, in_s);
	
	l = scm_c_string_length(in_s);
	SCM_SAFE_STRDUP(in_s, p);
	appendBinaryStringInfo(soft_port_si, p, l);

	return SCM_BOOL_T;
}
#undef FUNC_NAME

static SCM
soft_port_create(void)
{
	/* We'll need a vector of length 5 to make a soft-port. */
	SCM	pv = scm_c_make_vector(5, SCM_BOOL_F);

	/* 
	 * Because of we'll handle only output data, two functions for reading
	 * character and string data is enough for us.
	 */
	SCM	handler_c = scm_c_make_gsubr("internal soft_port_handler_c", 1, 0, 0,
									 soft_port_handler_c);
	SCM handler_s = scm_c_make_gsubr("internal soft_port_handler_s", 1, 0, 0,
									 soft_port_handler_s);

	scm_vector_set_x(pv, scm_from_int(0), handler_c);
	scm_vector_set_x(pv, scm_from_int(1), handler_s);
	
	return scm_make_soft_port(pv, scm_mem2string("w", 1));
}


/*
 * guile_init - Initializing guile related stuff before going any further.
 * 				Backtracing, user space code execution environment and data
 * 				conversion routines sort of pre-required steps go here.
 */
static void
guile_init(void)
{
	if (guile_init_level < 1)
	{
		/*
		 * Arrange things so that all of the code in the current thread
		 * executes in Guile mode.
		 */
		scm_init_guile();

		/* Empty backtrace stack. */
		guile_backtrace_stack = SCM_BOOL_F;

		/* Enable backtracing. */
		SCM_DEVAL_P = 1;
		SCM_BACKTRACE_P = 1;
		SCM_RECORD_POSITIONS_P = 1;
		SCM_RESET_DEBUG_MODE;

		guile_init_level = 1;
	}

	if (guile_init_level < 2)
	{
		/* Initialize user space code execution environment. */
		CALL_PROC_WITH_CATCH(guile_init_execution_module, NULL);
		
		guile_init_level = 2;
	}

	if (guile_init_level < 3)
	{
		/* Post-initialization stuff. (e.g. loading dataconv.scm) */
		CALL_PROC_WITH_CATCH(guile_init_post, NULL);

		guile_init_level = 3;
	}
}


/*
 * guile_init_execution_module - Initializes execution environment
 * 								 (guile_execution_module) for user space code.
 */
static SCM
guile_init_execution_module(void *data)
{
#ifdef SAFE_R5RS
/*
 * If SAFE_R5RS flag is turned on, then create a trusted PL. For this purpose,
 * we will use safe-r5rs module comes with Guile by default. And also we'll need
 * to export some utility functions that will be quite handy in some situations.
 */
#define EXPORT_FUNC(fn)	"(module-define! m '" fn " " fn ")"
#define CREATE_MODULE \
	"(use-modules (ice-9 safe-r5rs))" \
	"(let ((m (resolve-interface '(ice-9 safe-r5rs))))" \
	\
	/* Export row accessor functions. */ \
	EXPORT_FUNC("assoc-set!") \
	EXPORT_FUNC("assq") \
	EXPORT_FUNC("assv") \
	EXPORT_FUNC("assoc") \
	EXPORT_FUNC("assq-ref") \
	EXPORT_FUNC("assv-ref") \
	EXPORT_FUNC("assoc-ref") \
	EXPORT_FUNC("assq-remove!") \
	EXPORT_FUNC("assv-remove!") \
	EXPORT_FUNC("assoc-remove!") \
	\
	/* Return the interface. */ \
	"m)"
#else
/*
 * If this won't be a trusted PL, then create a child module using current
 * module interface.
 */
#define CREATE_MODULE	"(make-module 0 (list (current-module)))"
#endif

	guile_execution_module = scm_eval_string(scm_from_locale_string(CREATE_MODULE));

	return SCM_BOOL_T;
}


/*
 * guile_init_post - This is the next step to go after succesfully finishing
 * 					 guile_init(). Post initialization needs a seperate
 * 					 function to be called in a catch body.
 */
static SCM
guile_init_post(void *data)
{
	int		 ret;
	char	*fpath;

	/*
	 * We should check the file accessbility first. Because
	 * scm_primitive_load() does this in a very ugly way for us.
	 */
	BUILD_FILE_PATH(fpath, FILE_DATACONV);
	ret = access(fpath, R_OK);
	if (ret < 0)
	{
		int	eno = errno;
		scm_throw(scm_from_locale_symbol("access-fail"),
				  scm_list_3(scm_from_locale_string(fpath),
					  		 scm_from_int(eno),
					  		 scm_from_locale_string(strerror(eno))));
	}

	/* Load and locate type imposition/exposition functions. */
	scm_primitive_load(scm_from_locale_string(fpath));
	scm_data_impose = scm_variable_ref(scm_c_lookup(SCM_DATA_IMPOSE_FUNC));
	scm_data_expose = scm_variable_ref(scm_c_lookup(SCM_DATA_EXPOSE_FUNC));

	/* Per-session shared variable. */
	scm_c_define("pl-shared", SCM_EOL);

	return SCM_BOOL_T;
}


/*
 * guile_init_fly - Common initialization steps that needs to be done just
 * 					before every procedure evalution.
 */
static void
guile_init_fly(void)
{
	SCM	oldmodule;
	
	/* Validate execution module before the definitions. */
	Assert(guile_execution_module);
	
	/*
	 * Define exceptions.
	 */
	{
		int	i = 0;
		
#define DEFINE_EXCP(name, errcode) \
	do { \
		excptbl[i].code = errcode; \
		excptbl[i].sym = scm_from_locale_symbol(name); \
		scm_c_define(name, excptbl[i].sym); \
		i++; \
	} while (0)
		
#include "exceptions.c"
	}

	/*
	 * Define report levels and report function.
	 */
#define DEFINE_LEVEL(level, value) \
	scm_c_define(level, scm_from_int(value))

	DEFINE_LEVEL("debug-level",		REPORT_LEVEL_DEBUG);
	DEFINE_LEVEL("log-level",		REPORT_LEVEL_LOG);
	DEFINE_LEVEL("info-level",		REPORT_LEVEL_INFO);
	DEFINE_LEVEL("notice-level",	REPORT_LEVEL_NOTICE);
	DEFINE_LEVEL("warning-level",	REPORT_LEVEL_WARNING);
	DEFINE_LEVEL("exception-level",	REPORT_LEVEL_EXCEPTION);
	
	/*
	 * Make function definitions and load initialization script
	 * inside the execution module.
	 */
	oldmodule = scm_set_current_module(guile_execution_module);
	
	scm_c_define_gsubr("report", 2, 1, 0, report);

	/*
	 * Load initialization script file. (FILE_INIT)
	 */
	{
		int		 ret;
		char	*fpath;

		BUILD_FILE_PATH(fpath, FILE_INIT);
		ret = access(fpath, R_OK);
		if (ret < 0)
		{
			int eno = errno;
			scm_throw(scm_from_locale_symbol("access-fail"),
					  scm_list_3(scm_from_locale_string(fpath),
								 scm_from_int(eno),
								 scm_from_locale_string(strerror(eno))));
		}
		scm_primitive_load(scm_from_locale_string(fpath));
	}

	/* Return to previous execution environment. */
	scm_set_current_module(oldmodule);
}


/*
 * guile_cleanup - Cleanup routines for Guile objects.
 */
static void
guile_cleanup(void)
{
	/*
	 * In the previous versions, we were forcing Guile GC reclaiming in here.
	 * But after concerning more about it, I realized that it's not a very smart
	 * idea and it'd be best to let this stuff in the hands of GC. It's
	 * obviously smarter than us on deciding GC reclaiming time.
	 */
}


/*
 * guile_pre_unwind_handler - Create backtrace stack in the same
 * 							  context `throw' is called.
 */
static SCM
guile_pre_unwind_handler(void *data, SCM key, SCM params)
{
	/* Create backtrace stack. */
	guile_backtrace_stack = scm_make_stack(SCM_BOOL_T, SCM_EOL);

	return SCM_BOOL_T;
}


/*
 * guile_exception_handler - Display thrown exception and quit gently.
 * 							 (Backtrace stack should have been created by
 * 							 pre-unwind handler previously.)
 */
static SCM
guile_exception_handler(void *data, SCM tag, SCM args)
{
	SCM		 soft_port;
	char	*s;
	int		 l;
	
	/* Initialize Guile stderr redirection stuff. */
	soft_port_si = makeStringInfo();
	soft_port = soft_port_create();

	/*
	 * TODO: Actually, there's no guarantee soft_port_create() won't raise an
	 * exception. And in such a situation, it'll get handled by the same
	 * exception handler that will result in an endless loop. So, let's pray(!)
	 * for it to return success, unless somebody would manage to implement a
	 * statically allocated buffer that Guile can use to handle exceptions.
	 */

	/*
	 * Let Guile to take care of fancy error printing stuff while our
	 * soft-port handlers catches its output data.
	 */
	if (scm_length(args) == scm_from_int(4))
	{
		SCM	subr = SCM_CAR(args);
		SCM	message = SCM_CADR(args);
		SCM	parts = SCM_CADDR(args);
		SCM	rest = SCM_CADDDR(args);
		SCM	highlights = (scm_is_eq(tag, scm_arg_type_key) ||
						  scm_is_eq(tag, scm_out_of_range_key))
						 ? rest : SCM_EOL;

		/* Display backtrace. */
		scm_display_backtrace_with_highlights(guile_backtrace_stack, soft_port,
											  SCM_BOOL_F, SCM_BOOL_F, highlights);

		scm_puts("\n", soft_port);

		/* Display error messages. */
		scm_display_error(guile_backtrace_stack, soft_port, subr,
						  message, parts, rest);
	}
	else
	{
		char	*prog_name = (char *) data;

		if (!prog_name)
			prog_name = "guile";

		scm_puts(prog_name, soft_port);
		scm_puts(": ", soft_port);

		scm_puts("uncaught throw to ", soft_port);
		scm_prin1(tag, soft_port, 0);
		scm_puts(": ", soft_port);
		scm_prin1(args, soft_port, 1);
		scm_putc('\n', soft_port);
	}
	
	/*
	 * Throw Guile exception as a procedure exception.
	 */
	s = soft_port_si->data;
	l = soft_port_si->len;
	s[l - 1] = '\0';
	ereport(ERROR,
			(errcode(ERRCODE_SYNTAX_ERROR),
			 errmsg("Uncaugth exception thrown from PL/scheme procedure."),
			 errdetail("Guile error output:\n%s", s)));
	
	return SCM_UNSPECIFIED; /* Never reached. */
}


/*
 * guile_eval_str - Evaluates given Scheme code.
 */
static SCM
guile_eval_str(void *thunk)
{
	/* Issue on-the-fly initializations. */
	guile_init_fly();
	
	/*
	 * Evalute given Scheme code in the guile_execution_module environment.
	 * (See guile_init_execution_module() for more information about the
	 * environment.)
	 */
	return scm_eval_string_in_module(scm_from_locale_string((char *) thunk),
									 guile_execution_module);
}


/*
 * report - Report PostgreSQL error/exception. (ereport() wrapper.)
 */
static SCM
report(SCM in_level, SCM in_msg, SCM in_hint)
#define FUNC_NAME "report"
{
	int		 level;
	char	*msg;
	
	SCM_VALIDATE_EXACT(0, in_level);
	level = scm_to_int(in_level);
	SCM_ASSERT_RANGE(0, in_level, (level >= REPORT_LEVEL_DEBUG &&
								   level <= REPORT_LEVEL_EXCEPTION));
	level = report_level[level];

	SCM_VALIDATE_STRING(1, in_msg);
	SCM_SAFE_STRDUP(in_msg, msg);

	if (in_hint != SCM_UNDEFINED)
	{
		char	*hint;
		
		SCM_VALIDATE_STRING(2, in_hint);
		SCM_SAFE_STRDUP(in_hint, hint);

		ereport(level,
				(((level >= ERROR) ? errcode(ERRCODE_RAISE_EXCEPTION) : 0),
				 errmsg(msg), errhint(hint)));
	}
	else
		ereport(level,
				(((level >= ERROR) ? errcode(ERRCODE_RAISE_EXCEPTION) : 0),
				 errmsg(msg)));

	return in_level;
}
#undef FUNC_NAME


/*
 * Garbage collection routines for spi-plan-typ.
 */
static SCM
spi_plan_mark(SCM in_spiplan)
{
	/*
	 * spi-plan-typ will behave as a single structure invisible to the user.
	 * Programmer only needs to pass this variable to a spi-execute-prepared.
	 */
	return SCM_BOOL_F;
}


static size_t
spi_plan_free(SCM in_spiplan)
{
	spi_plan_t	*spiplan = (spi_plan_t *) SCM_SMOB_DATA(in_spiplan);
	
	/* Free plan first. */
	if (spi_conn_established)
		SPI_freeplan(spiplan->plan);
	
	/* Free typinputs and typioparams. */
	scm_gc_free(spiplan->typinputs, (spiplan->nargs * sizeof(Oid)),
				"SPI plan typinputs");
	scm_gc_free(spiplan->typioparams, (spiplan->nargs * sizeof(Oid)),
				"SPI plan typioparams");
	
	/* Lastly, free structure. */
	scm_gc_free(spiplan, sizeof(spi_plan_t), "SPI plan");
	
	return 0;
}


/*
 * spi_init - Initializes SPI stuff. (Ports SPI constants into Scheme
 * 			  code. Creates appropriate SMOBs for some internal SPI
 * 			  return values.)
 */
static SCM
spi_init(SCM data)
{
	static bool	constants_initialized = false;
	SCM			oldmodule;

	/*
	 * Port SPI constants once for every session.
	 */
	if (!constants_initialized)
	{
#define MAKE_SPI_SYM(indice, sym) \
		spisym[indice] = scm_from_locale_symbol(sym)

		MAKE_SPI_SYM(SPI_SYM_OK_SELECT,				"spi-ok-select");
		MAKE_SPI_SYM(SPI_SYM_OK_SELINTO,			"spi-ok-select-into");
		MAKE_SPI_SYM(SPI_SYM_OK_DELETE,				"spi-ok-delete");
		MAKE_SPI_SYM(SPI_SYM_OK_DELETE_RETURNING,	"spi-ok-delete-returning");
		MAKE_SPI_SYM(SPI_SYM_OK_INSERT,				"spi-ok-insert");
		MAKE_SPI_SYM(SPI_SYM_OK_INSERT_RETURNING,	"spi-ok-insert-returning");
		MAKE_SPI_SYM(SPI_SYM_OK_UPDATE,				"spi-ok-update");
		MAKE_SPI_SYM(SPI_SYM_OK_UPDATE_RETURNING,	"spi-ok-update-returning");
		MAKE_SPI_SYM(SPI_SYM_OK_UTILITY,			"spi-ok-utility");
		MAKE_SPI_SYM(SPI_SYM_ERROR_ARGUMENT,		"spi-error-argument");
		MAKE_SPI_SYM(SPI_SYM_ERROR_COPY,			"spi-error-copy");
		MAKE_SPI_SYM(SPI_SYM_ERROR_CURSOR,			"spi-error-cursor");
		MAKE_SPI_SYM(SPI_SYM_ERROR_TRANSACTION,		"spi-error-transaction");
		MAKE_SPI_SYM(SPI_SYM_ERROR_OPUNKNOWN,		"spi-error-opunknown");
		MAKE_SPI_SYM(SPI_SYM_ERROR_UNCONNECTED,		"spi-error-unconnected");

		constants_initialized = true;
	}

	/*
	 * Validate execution module and switch to it - to make function definitions
	 * go inside execution module.
	 */
	Assert(guile_execution_module);
	oldmodule = scm_set_current_module(guile_execution_module);

	/*
	 * Register spi-plan-typ type.
	 */
	typ_spi_plan = scm_make_smob_type("spi-plan", sizeof(spi_plan_t));
	scm_set_smob_mark(typ_spi_plan, spi_plan_mark);
	scm_set_smob_free(typ_spi_plan, spi_plan_free);

	/*
	 * Register ported SPI functions.
	 */
	scm_c_define_gsubr("spi-execute", 1, 1, 0, spi_execute);
	scm_c_define_gsubr("spi-prepare", 2, 0, 0, spi_prepare);
	scm_c_define_gsubr("spi-execute-prepared", 2, 1, 0, spi_execute_prepared);

	/* Switch back to previous module environment. */
	scm_set_current_module(oldmodule);

	return SCM_BOOL_T;
}


/*
 * convert_from_spi - Converts any SPI return constant into its Scheme
 *					  representation.
 */
static SCM
convert_from_spi(int ret)
{
#define RETURN_SPI_SYM(val, sym) \
	case val: return spisym[sym];
			  
	switch (ret)
	{
		RETURN_SPI_SYM(SPI_OK_SELECT,			SPI_SYM_OK_SELECT);
		RETURN_SPI_SYM(SPI_OK_SELINTO,			SPI_SYM_OK_SELINTO);
		RETURN_SPI_SYM(SPI_OK_DELETE,			SPI_SYM_OK_DELETE);
		RETURN_SPI_SYM(SPI_OK_DELETE_RETURNING,	SPI_SYM_OK_DELETE_RETURNING);
		RETURN_SPI_SYM(SPI_OK_INSERT,			SPI_SYM_OK_INSERT);
		RETURN_SPI_SYM(SPI_OK_INSERT_RETURNING,	SPI_SYM_OK_INSERT_RETURNING);
		RETURN_SPI_SYM(SPI_OK_UPDATE,			SPI_SYM_OK_UPDATE);
		RETURN_SPI_SYM(SPI_OK_UPDATE_RETURNING,	SPI_SYM_OK_UPDATE_RETURNING);
		RETURN_SPI_SYM(SPI_OK_UTILITY,			SPI_SYM_OK_UTILITY);
		RETURN_SPI_SYM(SPI_ERROR_ARGUMENT,		SPI_SYM_ERROR_ARGUMENT);
		RETURN_SPI_SYM(SPI_ERROR_COPY,			SPI_SYM_ERROR_COPY);
		RETURN_SPI_SYM(SPI_ERROR_CURSOR,		SPI_SYM_ERROR_CURSOR);
		RETURN_SPI_SYM(SPI_ERROR_TRANSACTION,	SPI_SYM_ERROR_TRANSACTION);
		RETURN_SPI_SYM(SPI_ERROR_OPUNKNOWN,		SPI_SYM_ERROR_OPUNKNOWN);
		RETURN_SPI_SYM(SPI_ERROR_UNCONNECTED,	SPI_SYM_ERROR_UNCONNECTED);

		default:
			elog(ERROR, "Couldn't recognize SPI return value! (%d)", ret);
	}
	
	return SCM_UNSPECIFIED; /* Will never reach here. */
}


/*
 * spi_return_tuples - Returns meta information (and tuples if available)
 * 					   by looking at SPI_tuptable.
 */
static SCM
spi_return_tuples(int ret, SCM throw_args)
{
	bool	has_affected_tuples;
	bool	has_returned_tuples;
	SCM		res;
	
	switch (ret)
	{
		case SPI_OK_INSERT_RETURNING:
		case SPI_OK_DELETE_RETURNING:
		case SPI_OK_UPDATE_RETURNING:
		case SPI_OK_SELECT:
			has_affected_tuples = false;
			has_returned_tuples = true;
			break;
			
		case SPI_OK_SELINTO:
		case SPI_OK_DELETE:
		case SPI_OK_INSERT:
		case SPI_OK_UPDATE:
			has_affected_tuples = true;
			has_returned_tuples = false;
			break;
		
		case SPI_OK_UTILITY:
			has_affected_tuples = false;
			has_returned_tuples = false;
			break;
			
		case SPI_ERROR_ARGUMENT:
		case SPI_ERROR_COPY:
		case SPI_ERROR_CURSOR:
		case SPI_ERROR_TRANSACTION:
		case SPI_ERROR_OPUNKNOWN:
		case SPI_ERROR_UNCONNECTED:
			scm_throw(convert_from_spi(ret), throw_args);
			break;
	}

	/* Assign status. */
	res = scm_acons(scm_from_locale_string("status"),
					convert_from_spi(ret),
					SCM_EOL);

	/* Do we have any affected tuples? */
	if (has_affected_tuples)
		res = scm_acons(scm_from_locale_string("affected-tuples"),
						scm_from_int(SPI_processed),
						res);
	
	/* Attach tuples returned from the query. */
	if (has_returned_tuples)
	{
		int			 ntuples = SPI_processed;
		int			 natts;
		SCM			 scm_tuples;
		HeapTuple	*tuples = SPI_tuptable->vals;
		TupleDesc	 tupdesc = SPI_tuptable->tupdesc;
		int			 i;

		scm_tuples = scm_c_make_vector(ntuples, SCM_UNSPECIFIED);

		/* Number of attributes a single tuple will hold. */
		for (natts = i = 0; i < tupdesc->natts; i++)
			if (!tupdesc->attrs[i]->attisdropped)
				natts++;

		for (i = 0; i < ntuples; i++)
		{
			HeapTuple	tuple = tuples[i];
			SCM			scm_tuple = scm_c_make_vector(natts, SCM_EOL);
			int			j, k;

			for (j = k = 0; j < tupdesc->natts; j++)
			{
				bool			 isnull;
				Datum			 attr;
				Oid				 atttypid = tupdesc->attrs[j]->atttypid;
				HeapTuple		 typetup;
				Form_pg_type	 typestruct;
				char			*outputstr;
				SCM				 val;
				
				if (tupdesc->attrs[j]->attisdropped)
					continue;

				attr = heap_getattr(tuple, (j + 1), tupdesc, &isnull);
				
				/* We already have filled whole tuple values with SCM_EOL. */
				if (isnull)
					continue;

				/* Look up for attribute's type properties. */
				typetup = SearchSysCache(TYPEOID,
										 ObjectIdGetDatum(atttypid),
										 0, 0, 0);
				if (!HeapTupleIsValid(typetup))
					elog(ERROR, "Cache lookup failed for type %d.", atttypid);
				typestruct = (Form_pg_type) GETSTRUCT(typetup);

				/* Get string representation of the attribute. */
				outputstr = OidOutputFunctionCall(typestruct->typoutput, attr);

				/* Filter it through data imposition function. */
				val = SCM_DATA_IMPOSE(outputstr, NameStr(typestruct->typname));

				ReleaseSysCache(typetup);

				/* Place produced result into the appropriate vector cell. */
				scm_c_vector_set_x(scm_tuple, k++, val);
			}

			scm_c_vector_set_x(scm_tuples, i, scm_tuple);
		}
		
		res = scm_acons(scm_from_locale_string("returned-tuples"),
						scm_vector_to_list(scm_tuples), res);
	}

	return res;
}


static SCM
spi_execute(SCM in_command, SCM in_count)
#define FUNC_NAME "spi-execute"
{
	long	 count = 0;
	char	*command;
	int		 ret;
	
	SCM_VALIDATE_STRING(0, in_command);
	SCM_SAFE_STRDUP(in_command, command);

	if (in_count != SCM_UNDEFINED)
	{
		SCM_VALIDATE_EXACT(1, in_count);
		count = scm_to_long(in_count);
		SCM_ASSERT_RANGE(1, in_count, (count >= 0));
	}

	GUILE_TRY();
	{
		ret = SPI_execute(command, proc->isreadonly, count);
	}
	GUILE_END_TRY();
	
	return spi_return_tuples(ret,
							 scm_list_3(scm_from_locale_string(FUNC_NAME),
								 		in_command, in_count));
}
#undef FUNC_NAME


static SCM
spi_prepare(SCM in_command, SCM in_argtypes)
#define FUNC_NAME "spi-prepare"
{
	char		*command;
	int			 i;
	int			 nargs;
	Oid			*argtypes;
	void		*ret;
	spi_plan_t	*spiplan;
	SCM			 smob;

	SCM_VALIDATE_STRING(0, in_command);
	SCM_SAFE_STRDUP(in_command, command);

	SCM_VALIDATE_VECTOR(1, in_argtypes);
	nargs = scm_c_vector_length(in_argtypes);
	SCM_ASSERT_RANGE(1, scm_from_int(nargs), (nargs >= 0));

	if (nargs > 0)
	{
		argtypes = palloc(nargs * sizeof(Oid));

		/*
		 * Receive OIDs of argument types.
		 */
		for (i = 0; i < nargs; i++)
		{
			SCM		 scm;
			char	*typname;
			int		 typmod;

			scm = scm_c_vector_ref(in_argtypes, i);
			SCM_VALIDATE_STRING(1, scm);
			SCM_SAFE_STRDUP(scm, typname);

			parseTypeString(typname, &argtypes[i], &typmod);
		}
	}
	else
		argtypes = NULL;

	GUILE_TRY();
	{
		ret = SPI_prepare(command, nargs, argtypes);
	}
	GUILE_END_TRY();
	
	if (!ret)
		scm_throw(convert_from_spi(SPI_result),
				  scm_list_3(scm_from_locale_string(FUNC_NAME),
					  		 in_command, in_argtypes));

	/*
	 * Move plan into a permanent memory context. Guile GC will handle
	 * it if needed. (Also, by this way, we avoid freeing a plan.
	 * Right now, plan is in the SPI procCxt, which will go away at
	 * function end.)
	 */
	GUILE_TRY();
	{
		void	*tmpret;

		tmpret = ret;
		ret = SPI_saveplan(tmpret);
		SPI_freeplan(tmpret);
	}
	GUILE_END_TRY();

	if (!ret)
		scm_throw(convert_from_spi(SPI_result),
				  scm_list_3(scm_from_locale_string(FUNC_NAME),
							 in_command, in_argtypes));

	/*
	 * Initialize SMOB to return to user.
	 */
	spiplan = scm_gc_malloc(sizeof(spi_plan_t), "SPI plan");
	spiplan->plan = ret;
	spiplan->nargs = nargs;

	SCM_NEWSMOB(smob, typ_spi_plan, spiplan);
	
	spiplan->typinputs = scm_gc_malloc((sizeof(Oid) * nargs),
									   "SPI plan typinputs");
	spiplan->typioparams = scm_gc_malloc((sizeof(Oid) * nargs),
										 "SPI plan typioparams");
	
	/*
	 * Collect typinput and typioparam of argument types. (We'll use
	 * these values while formating parameters in plan execution.)
	 */
	for (i = 0; i < nargs; i++)
		getTypeInputInfo(argtypes[i],
						 &spiplan->typinputs[i],
						 &spiplan->typioparams[i]);

	return smob;
}
#undef FUNC_NAME


static SCM
spi_execute_prepared(SCM in_spiplan, SCM in_args, SCM in_count)
#define FUNC_NAME "spi-execute-prepared"
{
	spi_plan_t	*spiplan;
	int			 nargs;
	Datum		*values;
	char		*nulls;
	long		 count = 0;
	int			 ret;

	SCM_VALIDATE_SPI_PLAN(in_spiplan);
	spiplan = (spi_plan_t *) SCM_SMOB_DATA(in_spiplan);

	SCM_VALIDATE_VECTOR(1, in_args);
	nargs = scm_c_vector_length(in_args);
	SCM_ASSERT_RANGE(1, scm_from_int(nargs), (nargs == spiplan->nargs));

	if (nargs > 0)
	{
		int	i;

		values = palloc(nargs * sizeof(Datum));
		nulls = palloc(nargs * sizeof(char));
		
		/*
		 * Convert each argument into corresponding Datum value.
		 */
		for (i = 0; i < nargs; i++)
		{
			SCM	arg = scm_c_vector_ref(in_args, i);

			/* NULL value. */
			if (scm_is_null(arg))
			{
				nulls[i] = 'n';
				values[i] = OidInputFunctionCall(spiplan->typinputs[i],
												 NULL, spiplan->typioparams[i],
												 -1);
			}
			else
			{
				char	*str;
			
				SCM_VALIDATE_STRING(1, arg);
				SCM_SAFE_STRDUP(arg, str);

				nulls[i] = ' ';
				values[i] = OidInputFunctionCall(spiplan->typinputs[i],
												 str, spiplan->typioparams[i],
												 -1);
			}
		}
	}
	else
	{
		values = NULL;
		nulls = NULL;
	}

	if (in_count != SCM_UNDEFINED)
	{
		SCM_VALIDATE_EXACT(3, in_count);
		count = scm_to_long(in_count);
		SCM_ASSERT_RANGE(1, in_count, (count >= 0));
	}
	
	GUILE_TRY();
	{
		ret = SPI_execute_plan(spiplan->plan, values, nulls,
							   proc->isreadonly, count);
	}
	GUILE_END_TRY();
	
	return spi_return_tuples(ret,
							 scm_list_4(scm_from_locale_string(FUNC_NAME),
								 		in_spiplan, in_args, in_count));
}
#undef FUNC_NAME
