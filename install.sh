#!/bin/sh
#
# PL/scheme Installation Script
#
# Copyright (c) 2006-2019, Volkan Yazıcı <vlkan.yazici@gmail.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
# - Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
# - Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#

# Where to report errors.
ERROR_LOG="error.log"

#
# Help menu.
#
display_usage()
{
	BNAME=`basename $0`
	cat <<EOF
Usage:"
  $BNAME [<options>]

Available Options:
  --pg-config <path>    pg_config executable's path.
  --module-dir <path>   Scheme modules directory.
                        (Default: pg_config --pkglibdir)
  --max-cache-size <n>  Maximum number of (non-volatile and non-SRF)
                        procedures to cache.
  --dbname <dbname>     Database that will be connected to install PL/scheme
                        into pg_pltemplate system catalog. (\`postgres' will be
                        used by default.)
  --safe-r5rs           Compile PL/scheme as a trusted PL by excluding untrusted
                        functions. (Barebone safe-r5rs module will be used.)
  --enable-dbacreate    Allows database owners to create language.
                        (Disabled by default.)

After successfully finishing the installation, you may as well use

  plscheme[u].module_dir
  plscheme[u].cache_max_size

GUC variables to alter the associated values on-the-fly. (Otherwise, values
passed to PL/scheme at the compile time will be used as default.)
EOF
}

# Reset options.
PG_CONFIG=
MODULE_DIR=
MAX_CACHE_SIZE=
DBNAME=
SAFE_R5RS=
DBACREATE=

#
# Parse passed parameters.
#
state=
while [ "x$1" != "x" ]; do
	if [ "$state" = "--pg-config" ]; then
		PG_CONFIG=$1
	elif [ "$state" = "--module-dir" ]; then
		MODULE_DIR=$1
	elif [ "$state" = "--max-cache-size" ]; then
		MAX_CACHE_SIZE=$1
	elif [ "$state" = "--dbname" ]; then
		DBNAME=$1
	else
		case "$1" in
			--help)
				display_usage
				exit 0
				;;

			--pg-config) state="--pg-config" ;;
			--dataconv) state="--dataconv" ;;
			--max-cache-size) state="--max-cache-size" ;;
			--dbname) state="--dbname" ;;
			--safe-r5rs) SAFE_R5RS=1; state= ;;
			--dbacreate) DBACREATE=1; state= ;;

			*)
				echo "Unknown option: $1"
				display_usage
				exit 1
				;;
		esac

		shift
		continue
	fi
	
	state=
	shift
done

if test \! -z "$state"; then
	echo "Unspecified option: $state"
	display_usage
	exit 1
fi


#
# Validate pg_config path.
#
if test -z "$PG_CONFIG"; then
	PG_CONFIG="`which pg_config`"
	if test -z "$PG_CONFIG"; then
		echo "Couldn't locate pg_config. You either have to add its path into"
		echo "your PATH variable or specify it via --pg-config option."
		exit 1
	fi
fi
PG_INCLUDEDIR="`$PG_CONFIG --includedir-server`"
PG_PKGLIBDIR="`$PG_CONFIG --pkglibdir`"


#
# Validate dataconv.scm path.
#
if test -z "$MODULE_DIR"; then
	MODULE_DIR=$PG_PKGLIBDIR
fi


#
# Validate max cache size.
#
if test -z "$MAX_CACHE_SIZE"; then
	MAX_CACHE_SIZE=64
else
	if test "$MAX_CACHE_SIZE" -lt 0; then
		echo "Maximum cache size has to be equal to or greater than 0."
		exit 1
	fi
fi


#
# Validate database name.
#
if test -z "$DBNAME"; then
	DBNAME="postgres"
fi


CPPFLAGS="-g -Wall -fpic -c -I$PG_INCLUDEDIR $CPPFLAGS"
LDFLAGS="-shared -lguile $LDFLAGS"
PSQL="`$PG_CONFIG --bindir`/psql $PSQL_OPTS $DBNAME"

echo "pg_config     : $PG_CONFIG"
echo "module-dir    : $MODULE_DIR"
echo "max-cache-size: $MAX_CACHE_SIZE"
echo "dbname        : $DBNAME"
echo -n "safe-r5rs     : "; [ $SAFE_R5RS ] && echo "YES" || echo "NO"
echo -n "dbacreate     : "; [ $DBACREATE ] && echo "YES" || echo "NO"
echo "PSQL          : $PSQL"
echo "CPPFLAGS      : $CPPFLAGS"
echo "LDFLAGS       : $LDFLAGS"
echo


dump_errors()
{
	RET=$1

	if test $RET -ne 0; then
		echo "failed!"
		echo

		LEN="`wc -l $ERROR_LOG | awk '{print $1}'`"
		if [ "$LEN" -gt 10 ]; then
			head -n 10 $ERROR_LOG
			echo "..."
			echo "(See $ERROR_LOG for details.)"
		else
			cat $ERROR_LOG
			rm -f $ERROR_LOG
		fi

		exit $RET
	else
		rm -f $ERROR_LOG
		echo "done."
	fi
}


#
# Common `u' prefix. (For [u]ntrusted stuff.)
#
[ -z $SAFE_R5RS ] && u="u"


#
# Object output files.
#
OBJ="plscheme${u}.o"
SO="plscheme${u}.so"


#
# Compile sources.
#
echo -n "Compiling... "
if [ $SAFE_R5RS ]
then
	cc $CPPFLAGS \
		-DSAFE_R5RS \
		-DMODULE_DIR=\"$MODULE_DIR\" \
		-DMAX_CACHE_SIZE=$MAX_CACHE_SIZE \
		-o $OBJ plscheme.c \
		1>$ERROR_LOG 2>&1
else
	cc $CPPFLAGS \
		-DMODULE_DIR=\"$MODULE_DIR\" \
		-DMAX_CACHE_SIZE=$MAX_CACHE_SIZE \
		-o $OBJ plscheme.c \
		1>$ERROR_LOG 2>&1
fi
dump_errors $?


#
# Link object with the shared libraries.
#
echo -n "Linking... "
cc $LDFLAGS -o $SO $OBJ \
1>$ERROR_LOG 2>&1
dump_errors $?


#
# Copy files.
#
echo "Copying files..."
set -e
echo "  $SO -> $PG_PKGLIBDIR"
cp $SO $PG_PKGLIBDIR
echo "  init.scm -> $MODULE_DIR"
cp init.scm $MODULE_DIR
echo "  dataconv.scm -> $MODULE_DIR"
cp dataconv.scm $MODULE_DIR
set +e


#
# Install language to database.
#
[ $SAFE_R5RS ] && istrusted="t" || istrusted="f"
[ $DBACREATE ] && dbacreate="t" || dbacreate="f"
echo "Installing language into pg_pltemplate... "
$PSQL -c "INSERT INTO pg_pltemplate \
              (tmplname, tmpltrusted, tmpldbacreate, tmplhandler, tmpllibrary)
            VALUES
              ('plscheme${u}', '${istrusted}', '${dbacreate}', \
               'plscheme${u}_call_handler', '\$libdir/plscheme${u}')"

# Abort if query is failed.
RET=$?
if [ $RET -ne 0 ]; then
	cat <<EOF
--------------------------------------------------------------------------	
SQL query failed while trying to introduce language into pg_pltemplate
system catalog!

Hint: If above query was failed because of a connection/authentication
      related problem, you can call installation script ($0)
	  by prepending your own psql options to the command. For instance,

        shell$ PSQL_OPTS="-U vy -d templeofthedog"

      Script will automatically try to use PSQL_OPTS environment variable
      while issuing installation query.
EOF
	
	exit $RET
fi


#
# Dump success message.
#
cat <<EOF
--------------------------------------------------------------------------
PL/scheme is succesfully compiled and introduced into pg_pltemplate system
catalog. You can start using PL/scheme in any database you wish just after
issuing "CREATE LANGUAGE plscheme$u" command while connected to the related
database.

For more information about PL/scheme project you can visit our homepage at
https://vlkan.com/plscheme/
EOF
