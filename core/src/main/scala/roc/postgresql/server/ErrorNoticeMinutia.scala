package roc
package postgresql
package server

/** The set of ErrorMessage and NoticeResponse Message fields
  * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
  */
private[postgresql] object ErrorNoticeMessageFields {
  val Severity: Char         = 'S'
  val Code: Char             = 'C'
  val Message: Char          = 'M'
  val Detail: Char           = 'D'
  val Hint: Char             = 'H'
  val Position: Char         = 'P'
  val InternalPosition: Char = 'p'
  val InternalQuery: Char    = 'q'
  val Where: Char            = 'W'
  val SchemaName: Char       = 's'
  val TableName: Char        = 't'
  val ColumnName: Char       = 'c'
  val DataTypeName: Char     = 'd'
  val ConstraintName: Char   = 'n'
  val File: Char             = 'F'
  val Line: Char             = 'L'
  val Routine: Char          = 'R'
}

/** The set of Postgresql Error Class Codes
  *
  * Postgresql Errors are categorized into specific categories of errors. For example,
  * an ErrorCode of 01000 falls under the Warning Class, as it starts with 01. A Postgresql
  * server may emit an Error Code that Roc does not understand, but it should still
  * be able to categorize it based off it's Error Class.
  * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]] for more
  *     information
  */
private[server] object ErrorClassCodes {
  val SuccessfulCompletion                    = "00"
  val Warning                                 = "01"
  val NoData                                  = "02"
  val SQLStatementNotYetComplete              = "03"
  val ConnectionException                     = "08"
  val TriggeredActionException                = "09"
  val FeatureNotSupported                     = "0A"
  val InvalidTransactionInitiation            = "0B"
  val LocatorException                        = "0F"
  val InvalidGrantor                          = "0L"
  val InvalidRoleSpecification                = "0P"
  val DiagnosisException                      = "0Z"
  val CaseNotFound                            = "20"
  val CardinalityViolation                    = "21"
  val DataException                           = "22"
  val IntegrityConstraintViolation            = "23"
  val InvalidCursorState                      = "24"
  val InvalidTransactionState                 = "25"
  val InvalidSQLStatementName                 = "26"
  val TriggeredDataChangeViolation            = "27"
  val InvalidAuthorizationSpecification       = "28"
  val DependentPrivilegeDescriptorsStillExist = "2B"
  val InvalidTransactionTermination           = "2D"
  val SQLRoutineException                     = "2F"
  val InvalidCursorName                       = "34"
  val ExternalRoutineException                = "38"
  val ExternalRoutineInvocationException      = "39"
  val SavepointException                      = "3B"
  val InvalidCatalogName                      = "3D"
  val InvalidSchemaName                       = "3F"
  val TransactionRollback                     = "40"
  val SyntaxErrorOrAccessRuleViolation        = "42"
  val WithCheckOptionViolation                = "44"
  val InsufficientResources                   = "53"
  val ProgramLimitExceeded                    = "54"
  val ObjectNotInPrerequisiteState            = "55"
  val OperatorIntervention                    = "57"
  val SystemError                             = "58"    // these are errors external to Postgres
  val ConfigurationFileError                  = "F0"
  val ForeignDataWrapperError                 = "HV"
  val PLpgSQLError                            = "P0"
  val InternalError                           = "XX"
}
