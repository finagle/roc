package roc
package postgresql
package server

import org.specs2._
import org.specs2.specification.core._

final class ErrorNoticeMinutiaSpec extends Specification with ScalaCheck { def is = s2"""

  ErrorNoticeMessageFields
    Severity must equal 'S'               ${ENMF().testSeverity}
    SQLSTATE Code must equal 'C'          ${ENMF().testCode}
    Message must equal 'M'                ${ENMF().testMessage}
    Hint must equal 'H'                   ${ENMF().testHint}
    Position must equal 'P'               ${ENMF().testPosition}
    InternalPosition must equal 'p'       ${ENMF().testInternalPosition}
    InternalQuery must equal 'q'          ${ENMF().testInternalQuery}
    Where must equal 'W'                  ${ENMF().testWhere}
    SchemaName must equal 's'             ${ENMF().testSchemaName}
    TableName must equal 't'              ${ENMF().testTableName}
    ColumnName must equal 'c'             ${ENMF().testColumnName}
    DataTypeName must equal 'd'           ${ENMF().testDataTypeName}
    ConstraintName must equal 'n'         ${ENMF().testConstraintName}
    File must equal 'F'                   ${ENMF().testFile}
    Line must equal 'L'                   ${ENMF().testLine}
    Routine must equal 'R'                ${ENMF().testRoutine}

  Error Class Codes
    SuccessCodes must contain all Success values          ${ECC().testSuccessCodes}
    WarningCodes must contain all Warning values          ${ECC().testWarningCodes}
    ErrorCodes must contain all Error values              ${ECC().testErrorCodes}

    Successful Completion must be '00'                    ${ECC().testSuccessfulCompletion}
    Warning must be '01'                                  ${ECC().testWarning}
    NoData must be '02'                                   ${ECC().testNoData}
    SQLStatementNotYetComplete must be '03'               ${ECC().testSQLStatementNotYetComplete}
    ConnectionException must be '08'                      ${ECC().testConnectionException}
    TriggeredActionException must be '09'                 ${ECC().testTriggeredActionException}
    FeatureNotSupported must be '0A'                      ${ECC().testFeatureNotSupported}
    InvalidTransactionInitiation must be '0B'             ${ECC().testInvalidTransactionInitiation}
    LocatorException must be '0F'                         ${ECC().testLocatorException}
    InvalidGrantor must be '0L'                           ${ECC().testInvalidGrantor}
    InvalidRoleSpecification must be '0P'                 ${ECC().testInvalidRoleSpecification}
    DiagnosisException must be '0Z'                       ${ECC().testDiagnosisException}
    CaseNotFound must be '20'                             ${ECC().testCaseNotFound}
    CardinalityViolation must be '21'                     ${ECC().testCardinalityViolation}
    DataException must be '22'                            ${ECC().testDataException}
    IntegrityConstraintViolation must be '23'             ${ECC().testIntegrityConstraintViolation}
    InvalidCursorState must be '24'                       ${ECC().testInvalidCursorState}
    InvalidTransactionState must be '25'                  ${ECC().testInvalidTransactionState}
    InvalidSQLStatementName must be '26'                  ${ECC().testInvalidSQLStatementName}
    TriggeredDataChangeViolation must be '27'             ${ECC().testTriggeredDataChangeViolation}
    InvalidAuthorizationSpecification must be '28'        ${ECC().testInvalidAuthorizationSpecification}
    DependentPrivilegeDescriptorsStillExist must be '2B'  ${ECC().testDependentPrivilegeDescriptorsStillExist}
    InvalidTransactionTermination must be '2D'            ${ECC().testInvalidTransactionTermination}
    SQLRoutineException must be '2F'                      ${ECC().testSQLRoutineException}
    InvalidCursorName must be '34'                        ${ECC().testInvalidCursorName}
    ExternalRoutineException must be '38'                 ${ECC().testExternalRoutineException}
    ExternalRoutineInvocationException must be '39'       ${ECC().testExternalRoutineInvocationException}
    SavepointException must be '3B'                       ${ECC().testSavepointException}
    InvalidCatalogName must be '3D'                       ${ECC().testInvalidCatalogName}
    InvalidSchemaName must be '3F'                        ${ECC().testInvalidSchemaName}
    TransactionRollback must be '40'                      ${ECC().testTransactionRollback}
    SyntaxErrorOrAccessRuleViolation must be '42'         ${ECC().testSyntaxErrorOrAccessRuleViolation}
    WithCheckOptionViolation must be '44'                 ${ECC().testWithCheckOptionViolation}
    InsufficientResources must be '53'                    ${ECC().testInsufficientResources}
    ProgramLimitExceeded must be '54'                     ${ECC().testProgramLimitExceeded}
    ObjectNotInPrerequisiteState must be '55'             ${ECC().testObjectNotInPrerequisiteState}
    OperatorIntervention must be '57'                     ${ECC().testOperatorIntervention}
    SystemError must be '58'                              ${ECC().testSystemError}
    ConfigurationFileError must be 'F0'                   ${ECC().testConfigurationFileError}
    ForeignDataWrapperError must be 'HV'                  ${ECC().testForeignDataWrapperError}
    PLpgSQLError must be 'P0'                             ${ECC().testPLpgSQLError}
    InternalError must be 'XX'                            ${ECC().testInternalError}
                                                                                      """

  case class ENMF() extends ScalaCheck {
    import ErrorNoticeMessageFields._

    def testSeverity         = Severity must_== 'S'
    def testCode             = Code must_== 'C'
    def testMessage          = Message must_== 'M'
    def testHint             = Hint must_== 'H'
    def testPosition         = Position must_== 'P'
    def testInternalPosition = InternalPosition must_== 'p'
    def testInternalQuery    = InternalQuery must_== 'q'
    def testWhere            = Where must_== 'W'
    def testSchemaName       = SchemaName must_== 's'
    def testTableName        = TableName must_== 't'
    def testColumnName       = ColumnName must_== 'c'
    def testDataTypeName     = DataTypeName must_== 'd'
    def testConstraintName   = ConstraintName must_== 'n'
    def testFile             = File must_== 'F'
    def testLine             = Line must_== 'L'
    def testRoutine          = Routine must_== 'R'
  }

  case class ECC() extends ScalaCheck {
    import ErrorClassCodes._

    def testSuccessfulCompletion = SuccessfulCompletion must_== "00"
    def testWarning = Warning must_== "01"
    def testNoData = NoData must_== "02"
    def testSQLStatementNotYetComplete = SQLStatementNotYetComplete must_== "03"
    def testConnectionException = ConnectionException must_== "08"
    def testTriggeredActionException = TriggeredActionException must_== "09"
    def testFeatureNotSupported = FeatureNotSupported must_== "0A"
    def testInvalidTransactionInitiation = InvalidTransactionInitiation must_== "0B"
    def testLocatorException = LocatorException must_== "0F"
    def testInvalidGrantor = InvalidGrantor must_== "0L"
    def testInvalidRoleSpecification = InvalidRoleSpecification must_== "0P"
    def testDiagnosisException = DiagnosisException must_== "0Z"
    def testCaseNotFound = CaseNotFound must_== "20"
    def testCardinalityViolation = CardinalityViolation must_== "21"
    def testDataException = DataException must_== "22"
    def testIntegrityConstraintViolation = IntegrityConstraintViolation must_== "23"
    def testInvalidCursorState = InvalidCursorState must_== "24"
    def testInvalidTransactionState = InvalidTransactionState must_== "25"
    def testInvalidSQLStatementName = InvalidSQLStatementName must_== "26"
    def testTriggeredDataChangeViolation = TriggeredDataChangeViolation must_== "27"
    def testInvalidAuthorizationSpecification = InvalidAuthorizationSpecification must_== "28"
    def testDependentPrivilegeDescriptorsStillExist = 
      DependentPrivilegeDescriptorsStillExist must_== "2B"
    def testInvalidTransactionTermination = InvalidTransactionTermination must_== "2D"
    def testSQLRoutineException = SQLRoutineException must_== "2F"
    def testInvalidCursorName = InvalidCursorName must_== "34"
    def testExternalRoutineException = ExternalRoutineException must_== "38"
    def testExternalRoutineInvocationException = ExternalRoutineInvocationException must_== "39"
    def testSavepointException = SavepointException must_== "3B"
    def testInvalidCatalogName = InvalidCatalogName must_== "3D"
    def testInvalidSchemaName = InvalidSchemaName must_== "3F"
    def testTransactionRollback = TransactionRollback must_== "40"
    def testSyntaxErrorOrAccessRuleViolation = SyntaxErrorOrAccessRuleViolation must_== "42"
    def testWithCheckOptionViolation = WithCheckOptionViolation must_== "44"
    def testInsufficientResources = InsufficientResources must_== "53"
    def testProgramLimitExceeded = ProgramLimitExceeded must_== "54"
    def testObjectNotInPrerequisiteState = ObjectNotInPrerequisiteState must_== "55"
    def testOperatorIntervention = OperatorIntervention must_== "57"
    def testSystemError = SystemError must_== "58"
    def testConfigurationFileError = ConfigurationFileError must_== "F0"
    def testForeignDataWrapperError = ForeignDataWrapperError must_== "HV"
    def testPLpgSQLError = PLpgSQLError must_== "P0"
    def testInternalError = InternalError must_== "XX"

    def testSuccessCodes = SuccessCodes must_== List(SuccessfulCompletion)
    def testWarningCodes = WarningCodes must_== List(Warning, NoData)
    def testErrorCodes = {
      val expectedCodes = List(SQLStatementNotYetComplete, ConnectionException, 
        TriggeredActionException, FeatureNotSupported, InvalidTransactionInitiation,
        LocatorException,  InvalidGrantor, InvalidRoleSpecification,  DiagnosisException, 
        CaseNotFound, CardinalityViolation, DataException, IntegrityConstraintViolation, 
        InvalidCursorState, InvalidTransactionState, InvalidSQLStatementName, 
        TriggeredDataChangeViolation, InvalidAuthorizationSpecification, 
        DependentPrivilegeDescriptorsStillExist, InvalidTransactionTermination, SQLRoutineException,
        InvalidCursorName, ExternalRoutineException, ExternalRoutineInvocationException, 
        SavepointException, InvalidCatalogName, InvalidSchemaName, TransactionRollback, 
        SyntaxErrorOrAccessRuleViolation, WithCheckOptionViolation, InsufficientResources, 
        ProgramLimitExceeded, ObjectNotInPrerequisiteState, OperatorIntervention, SystemError, 
        ConfigurationFileError, ForeignDataWrapperError, PLpgSQLError, InternalError)
      ErrorCodes must_== expectedCodes
    }
  }
}

