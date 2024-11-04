/*
 * PL/scheme Exception Definitions
 *
 * Copyright (c) 2006-2024, Volkan Yazıcı <volkan@yazi.ci>
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


/*
 * Success and warnings can't be caught, so omit them from table.
 */

DEFINE_EXCP("sql-statement-not-yet-complete",	ERRCODE_SQL_STATEMENT_NOT_YET_COMPLETE);
DEFINE_EXCP("connection-exception",				ERRCODE_CONNECTION_EXCEPTION);
DEFINE_EXCP("connection-does-not-exist",		ERRCODE_CONNECTION_DOES_NOT_EXIST);
DEFINE_EXCP("connection-failure",				ERRCODE_CONNECTION_FAILURE);
DEFINE_EXCP("sqlclient-unable-to-establish-sqlconnection",
												ERRCODE_SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION);
DEFINE_EXCP("sqlserver-rejected-establishment-of-sqlconnection",
												ERRCODE_SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION);
DEFINE_EXCP("transaction-resolution-unknown",	ERRCODE_TRANSACTION_RESOLUTION_UNKNOWN);
DEFINE_EXCP("protocol-violation",				ERRCODE_PROTOCOL_VIOLATION);
DEFINE_EXCP("triggered-action-exception",		ERRCODE_TRIGGERED_ACTION_EXCEPTION);
DEFINE_EXCP("feature-not-supported",			ERRCODE_FEATURE_NOT_SUPPORTED);
DEFINE_EXCP("invalid-transaction-initiation",	ERRCODE_INVALID_TRANSACTION_INITIATION);
DEFINE_EXCP("locator-exception",				ERRCODE_LOCATOR_EXCEPTION);
DEFINE_EXCP("invalid-locator-specification",	ERRCODE_L_E_INVALID_SPECIFICATION);
DEFINE_EXCP("invalid-grantor",					ERRCODE_INVALID_GRANTOR);
DEFINE_EXCP("invalid-grant-operation",			ERRCODE_INVALID_GRANT_OPERATION);
DEFINE_EXCP("invalid-role-specification",		ERRCODE_INVALID_ROLE_SPECIFICATION);
DEFINE_EXCP("cardinality-violation",			ERRCODE_CARDINALITY_VIOLATION);
DEFINE_EXCP("data-exception",					ERRCODE_DATA_EXCEPTION);
DEFINE_EXCP("array-element-error",				ERRCODE_ARRAY_ELEMENT_ERROR);
DEFINE_EXCP("array-subscript-error",			ERRCODE_ARRAY_SUBSCRIPT_ERROR);
DEFINE_EXCP("character-not-in-repertoire",		ERRCODE_CHARACTER_NOT_IN_REPERTOIRE);
DEFINE_EXCP("datetime-field-overflow",			ERRCODE_DATETIME_FIELD_OVERFLOW);
DEFINE_EXCP("datetime-value-out-of-range",		ERRCODE_DATETIME_VALUE_OUT_OF_RANGE);
DEFINE_EXCP("division-by-zero",					ERRCODE_DIVISION_BY_ZERO);
DEFINE_EXCP("error-in-assignment",				ERRCODE_ERROR_IN_ASSIGNMENT);
DEFINE_EXCP("escape-character-conflict",		ERRCODE_ESCAPE_CHARACTER_CONFLICT);
DEFINE_EXCP("indicator-overflow",				ERRCODE_INDICATOR_OVERFLOW);
DEFINE_EXCP("interval-field-overflow",			ERRCODE_INTERVAL_FIELD_OVERFLOW);
DEFINE_EXCP("invalid-argument-for-logarithm",	ERRCODE_INVALID_ARGUMENT_FOR_LOG);
DEFINE_EXCP("invalid-argument-for-power-function",
												ERRCODE_INVALID_ARGUMENT_FOR_POWER_FUNCTION);
DEFINE_EXCP("invalid-argument-for-width-bucket-function",
												ERRCODE_INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION);
DEFINE_EXCP("invalid-character-value-for-cast",	ERRCODE_INVALID_CHARACTER_VALUE_FOR_CAST);
DEFINE_EXCP("invalid-datetime-format",			ERRCODE_INVALID_DATETIME_FORMAT);
DEFINE_EXCP("invalid-escape-character",			ERRCODE_INVALID_ESCAPE_CHARACTER);
DEFINE_EXCP("invalid-escape-octet",				ERRCODE_INVALID_ESCAPE_OCTET);
DEFINE_EXCP("invalid-escape-sequence",			ERRCODE_INVALID_ESCAPE_SEQUENCE);
DEFINE_EXCP("invalid-indicator-parameter-value",
												ERRCODE_INVALID_INDICATOR_PARAMETER_VALUE);
DEFINE_EXCP("invalid-limit-value",				ERRCODE_INVALID_LIMIT_VALUE);
DEFINE_EXCP("invalid-parameter-value",			ERRCODE_INVALID_PARAMETER_VALUE);
DEFINE_EXCP("invalid-regular-expression",		ERRCODE_INVALID_REGULAR_EXPRESSION);
DEFINE_EXCP("invalid-time-zone-displacement-value",
												ERRCODE_INVALID_TIME_ZONE_DISPLACEMENT_VALUE);
DEFINE_EXCP("invalid-use-of-escape-character",	ERRCODE_INVALID_USE_OF_ESCAPE_CHARACTER);
DEFINE_EXCP("most-specific-type-mismatch",		ERRCODE_MOST_SPECIFIC_TYPE_MISMATCH);
DEFINE_EXCP("null-value-not-allowed",			ERRCODE_NULL_VALUE_NOT_ALLOWED);
DEFINE_EXCP("null-value-no-indicator-parameter",
												ERRCODE_NULL_VALUE_NO_INDICATOR_PARAMETER);
DEFINE_EXCP("numeric-value-out-of-range",		ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE);
DEFINE_EXCP("string-data-length-mismatch",		ERRCODE_STRING_DATA_LENGTH_MISMATCH);
DEFINE_EXCP("string-data-right-truncation",		ERRCODE_STRING_DATA_RIGHT_TRUNCATION);
DEFINE_EXCP("substring-error",					ERRCODE_SUBSTRING_ERROR);
DEFINE_EXCP("trim-error",						ERRCODE_TRIM_ERROR);
DEFINE_EXCP("unterminated-c-string",			ERRCODE_UNTERMINATED_C_STRING);
DEFINE_EXCP("zero-length-character-string",		ERRCODE_ZERO_LENGTH_CHARACTER_STRING);
DEFINE_EXCP("floating-point-exception",			ERRCODE_FLOATING_POINT_EXCEPTION);
DEFINE_EXCP("invalid-text-representation",		ERRCODE_INVALID_TEXT_REPRESENTATION);
DEFINE_EXCP("invalid-binary-representation",	ERRCODE_INVALID_BINARY_REPRESENTATION);
DEFINE_EXCP("bad-copy-file-format",				ERRCODE_BAD_COPY_FILE_FORMAT);
DEFINE_EXCP("untranslatable-character",			ERRCODE_UNTRANSLATABLE_CHARACTER);
DEFINE_EXCP("integrity-constraint-violation",	ERRCODE_INTEGRITY_CONSTRAINT_VIOLATION);
DEFINE_EXCP("restrict-violation",				ERRCODE_RESTRICT_VIOLATION);
DEFINE_EXCP("not-null-violation",				ERRCODE_NOT_NULL_VIOLATION);
DEFINE_EXCP("foreign-key-violation",			ERRCODE_FOREIGN_KEY_VIOLATION);
DEFINE_EXCP("unique-violation",					ERRCODE_UNIQUE_VIOLATION);
DEFINE_EXCP("check-violation",					ERRCODE_CHECK_VIOLATION);
DEFINE_EXCP("invalid-cursor-state",				ERRCODE_INVALID_CURSOR_STATE);
DEFINE_EXCP("invalid-transaction-state",		ERRCODE_INVALID_TRANSACTION_STATE);
DEFINE_EXCP("active-sql-transaction",			ERRCODE_ACTIVE_SQL_TRANSACTION);
DEFINE_EXCP("branch-transaction-already-active",
												ERRCODE_BRANCH_TRANSACTION_ALREADY_ACTIVE);
DEFINE_EXCP("held-cursor-requires-same-isolation-level",
												ERRCODE_HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL);
DEFINE_EXCP("inappropriate-access-mode-for-branch-transaction",
												ERRCODE_INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION);
DEFINE_EXCP("inappropriate-isolation-level-for-branch-transaction",
												ERRCODE_INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION);
DEFINE_EXCP("no-active-sql-transaction-for-branch-transaction",
												ERRCODE_NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION);
DEFINE_EXCP("read-only-sql-transaction",		ERRCODE_READ_ONLY_SQL_TRANSACTION);
DEFINE_EXCP("schema-and-data-statement-mixing-not-supported",
												ERRCODE_SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED);
DEFINE_EXCP("no-active-sql-transaction",		ERRCODE_NO_ACTIVE_SQL_TRANSACTION);
DEFINE_EXCP("in-failed-sql-transaction",		ERRCODE_IN_FAILED_SQL_TRANSACTION);
DEFINE_EXCP("invalid-sql-statement-name",		ERRCODE_INVALID_SQL_STATEMENT_NAME);
DEFINE_EXCP("triggered-data-change-violation",	ERRCODE_TRIGGERED_DATA_CHANGE_VIOLATION);
DEFINE_EXCP("invalid-authorization-specification",
												ERRCODE_INVALID_AUTHORIZATION_SPECIFICATION);
DEFINE_EXCP("dependent-privilege-descriptors-still-exist",
												ERRCODE_DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST);
DEFINE_EXCP("dependent-objects-still-exist",	ERRCODE_DEPENDENT_OBJECTS_STILL_EXIST);
DEFINE_EXCP("invalid-transaction-termination",	ERRCODE_INVALID_TRANSACTION_TERMINATION);
DEFINE_EXCP("sql-routine-exception",			ERRCODE_SQL_ROUTINE_EXCEPTION);
DEFINE_EXCP("function-executed-no-return-statement",
												ERRCODE_S_R_E_FUNCTION_EXECUTED_NO_RETURN_STATEMENT);
DEFINE_EXCP("modifying-sql-data-not-permitted",	ERRCODE_S_R_E_MODIFYING_SQL_DATA_NOT_PERMITTED);
DEFINE_EXCP("prohibited-sql-statement-attempted",
												ERRCODE_S_R_E_PROHIBITED_SQL_STATEMENT_ATTEMPTED);
DEFINE_EXCP("reading-sql-data-not-permitted",	ERRCODE_S_R_E_READING_SQL_DATA_NOT_PERMITTED);
DEFINE_EXCP("invalid-cursor-name",				ERRCODE_INVALID_CURSOR_NAME);
DEFINE_EXCP("external-routine-exception",		ERRCODE_EXTERNAL_ROUTINE_EXCEPTION);
DEFINE_EXCP("containing-sql-not-permitted",		ERRCODE_E_R_E_CONTAINING_SQL_NOT_PERMITTED);
DEFINE_EXCP("modifying-sql-data-not-permitted",	ERRCODE_E_R_E_MODIFYING_SQL_DATA_NOT_PERMITTED);
DEFINE_EXCP("prohibited-sql-statement-attempted",
												ERRCODE_E_R_E_PROHIBITED_SQL_STATEMENT_ATTEMPTED);
DEFINE_EXCP("reading-sql-data-not-permitted",	ERRCODE_E_R_E_READING_SQL_DATA_NOT_PERMITTED);
DEFINE_EXCP("external-routine-invocation-exception",
												ERRCODE_EXTERNAL_ROUTINE_INVOCATION_EXCEPTION);
DEFINE_EXCP("invalid-sqlstate-returned",		ERRCODE_E_R_I_E_INVALID_SQLSTATE_RETURNED);
DEFINE_EXCP("null-value-not-allowed",			ERRCODE_E_R_I_E_NULL_VALUE_NOT_ALLOWED);
DEFINE_EXCP("trigger-protocol-violated",		ERRCODE_E_R_I_E_TRIGGER_PROTOCOL_VIOLATED);
DEFINE_EXCP("srf-protocol-violated",			ERRCODE_E_R_I_E_SRF_PROTOCOL_VIOLATED);
DEFINE_EXCP("savepoint-exception",				ERRCODE_SAVEPOINT_EXCEPTION);
DEFINE_EXCP("invalid-savepoint-specification",	ERRCODE_S_E_INVALID_SPECIFICATION);
DEFINE_EXCP("invalid-catalog-name",				ERRCODE_INVALID_CATALOG_NAME);
DEFINE_EXCP("invalid-schema-name",				ERRCODE_INVALID_SCHEMA_NAME);
DEFINE_EXCP("transaction-rollback",				ERRCODE_TRANSACTION_ROLLBACK);
DEFINE_EXCP("transaction-integrity-constraint-violation",
												ERRCODE_T_R_INTEGRITY_CONSTRAINT_VIOLATION);
DEFINE_EXCP("serialization-failure",			ERRCODE_T_R_SERIALIZATION_FAILURE);
DEFINE_EXCP("statement-completion-unknown",		ERRCODE_T_R_STATEMENT_COMPLETION_UNKNOWN);
DEFINE_EXCP("deadlock-detected",				ERRCODE_T_R_DEADLOCK_DETECTED);
DEFINE_EXCP("syntax-error-or-access-rule-violation",
												ERRCODE_SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION);
DEFINE_EXCP("syntax-error",						ERRCODE_SYNTAX_ERROR);
DEFINE_EXCP("insufficient-privilege",			ERRCODE_INSUFFICIENT_PRIVILEGE);
DEFINE_EXCP("cannot-coerce",					ERRCODE_CANNOT_COERCE);
DEFINE_EXCP("grouping-error",					ERRCODE_GROUPING_ERROR);
DEFINE_EXCP("invalid-foreign-key",				ERRCODE_INVALID_FOREIGN_KEY);
DEFINE_EXCP("invalid-name",						ERRCODE_INVALID_NAME);
DEFINE_EXCP("name-too-long",					ERRCODE_NAME_TOO_LONG);
DEFINE_EXCP("reserved-name",					ERRCODE_RESERVED_NAME);
DEFINE_EXCP("datatype-mismatch",				ERRCODE_DATATYPE_MISMATCH);
DEFINE_EXCP("indeterminate-datatype",			ERRCODE_INDETERMINATE_DATATYPE);
DEFINE_EXCP("wrong-object-type",				ERRCODE_WRONG_OBJECT_TYPE);
DEFINE_EXCP("undefined-column",					ERRCODE_UNDEFINED_COLUMN);
DEFINE_EXCP("undefined-cursor",					ERRCODE_UNDEFINED_CURSOR);
DEFINE_EXCP("undefined-database",				ERRCODE_UNDEFINED_DATABASE);
DEFINE_EXCP("undefined-function",				ERRCODE_UNDEFINED_FUNCTION);
DEFINE_EXCP("undefined-pstatement",				ERRCODE_UNDEFINED_PSTATEMENT);
DEFINE_EXCP("undefined-schema",					ERRCODE_UNDEFINED_SCHEMA);
DEFINE_EXCP("undefined-table",					ERRCODE_UNDEFINED_TABLE);
DEFINE_EXCP("undefined-parameter",				ERRCODE_UNDEFINED_PARAMETER);
DEFINE_EXCP("undefined-object",					ERRCODE_UNDEFINED_OBJECT);
DEFINE_EXCP("duplicate-column",					ERRCODE_DUPLICATE_COLUMN);
DEFINE_EXCP("duplicate-cursor",					ERRCODE_DUPLICATE_CURSOR);
DEFINE_EXCP("duplicate-database",				ERRCODE_DUPLICATE_DATABASE);
DEFINE_EXCP("duplicate-function",				ERRCODE_DUPLICATE_FUNCTION);
DEFINE_EXCP("duplicate-prepared-statement",		ERRCODE_DUPLICATE_PSTATEMENT);
DEFINE_EXCP("duplicate-schema",					ERRCODE_DUPLICATE_SCHEMA);
DEFINE_EXCP("duplicate-table",					ERRCODE_DUPLICATE_TABLE);
DEFINE_EXCP("duplicate-alias",					ERRCODE_DUPLICATE_ALIAS);
DEFINE_EXCP("duplicate-object",					ERRCODE_DUPLICATE_OBJECT);
DEFINE_EXCP("ambiguous-column",					ERRCODE_AMBIGUOUS_COLUMN);
DEFINE_EXCP("ambiguous-function",				ERRCODE_AMBIGUOUS_FUNCTION);
DEFINE_EXCP("ambiguous-parameter",				ERRCODE_AMBIGUOUS_PARAMETER);
DEFINE_EXCP("ambiguous-alias",					ERRCODE_AMBIGUOUS_ALIAS);
DEFINE_EXCP("invalid-column-reference",			ERRCODE_INVALID_COLUMN_REFERENCE);
DEFINE_EXCP("invalid-column-definition",		ERRCODE_INVALID_COLUMN_DEFINITION);
DEFINE_EXCP("invalid-cursor-definition",		ERRCODE_INVALID_CURSOR_DEFINITION);
DEFINE_EXCP("invalid-database-definition",		ERRCODE_INVALID_DATABASE_DEFINITION);
DEFINE_EXCP("invalid-function-definition",		ERRCODE_INVALID_FUNCTION_DEFINITION);
DEFINE_EXCP("invalid-prepared-statement-definition",
												ERRCODE_INVALID_PSTATEMENT_DEFINITION);
DEFINE_EXCP("invalid-schema-definition",		ERRCODE_INVALID_SCHEMA_DEFINITION);
DEFINE_EXCP("invalid-table-definition",			ERRCODE_INVALID_TABLE_DEFINITION);
DEFINE_EXCP("invalid-object-definition",		ERRCODE_INVALID_OBJECT_DEFINITION);
DEFINE_EXCP("with-check-option-violation",		ERRCODE_WITH_CHECK_OPTION_VIOLATION);
DEFINE_EXCP("insufficient-resources",			ERRCODE_INSUFFICIENT_RESOURCES);
DEFINE_EXCP("disk-full",						ERRCODE_DISK_FULL);
DEFINE_EXCP("out-of-memory",					ERRCODE_OUT_OF_MEMORY);
DEFINE_EXCP("too-many-connections",				ERRCODE_TOO_MANY_CONNECTIONS);
DEFINE_EXCP("program-limit-exceeded",			ERRCODE_PROGRAM_LIMIT_EXCEEDED);
DEFINE_EXCP("statement-too-complex",			ERRCODE_STATEMENT_TOO_COMPLEX);
DEFINE_EXCP("too-many-columns",					ERRCODE_TOO_MANY_COLUMNS);
DEFINE_EXCP("too-many-arguments",				ERRCODE_TOO_MANY_ARGUMENTS);
DEFINE_EXCP("object-not-in-prerequisite-state",	ERRCODE_OBJECT_NOT_IN_PREREQUISITE_STATE);
DEFINE_EXCP("object-in-use",					ERRCODE_OBJECT_IN_USE);
DEFINE_EXCP("cant-change-runtime-param",		ERRCODE_CANT_CHANGE_RUNTIME_PARAM);
DEFINE_EXCP("lock-not-available",				ERRCODE_LOCK_NOT_AVAILABLE);
DEFINE_EXCP("operator-intervention",			ERRCODE_OPERATOR_INTERVENTION);
DEFINE_EXCP("query-canceled",					ERRCODE_QUERY_CANCELED);
DEFINE_EXCP("admin-shutdown",					ERRCODE_ADMIN_SHUTDOWN);
DEFINE_EXCP("crash-shutdown",					ERRCODE_CRASH_SHUTDOWN);
DEFINE_EXCP("cannot-connect-now",				ERRCODE_CANNOT_CONNECT_NOW);
DEFINE_EXCP("io-error",							ERRCODE_IO_ERROR);
DEFINE_EXCP("undefined-file",					ERRCODE_UNDEFINED_FILE);
DEFINE_EXCP("duplicate-file",					ERRCODE_DUPLICATE_FILE);
DEFINE_EXCP("config-file-error",				ERRCODE_CONFIG_FILE_ERROR);
DEFINE_EXCP("lock-file-exists",					ERRCODE_LOCK_FILE_EXISTS);
DEFINE_EXCP("plpgsql-error",					ERRCODE_PLPGSQL_ERROR);
DEFINE_EXCP("raise-exception",					ERRCODE_RAISE_EXCEPTION);
DEFINE_EXCP("no-data-found",					ERRCODE_NO_DATA_FOUND);
DEFINE_EXCP("too-many-rows",					ERRCODE_TOO_MANY_ROWS);
DEFINE_EXCP("internal-error",					ERRCODE_INTERNAL_ERROR);
DEFINE_EXCP("data-corrupted",					ERRCODE_DATA_CORRUPTED);
DEFINE_EXCP("index-corrupted",					ERRCODE_INDEX_CORRUPTED);
