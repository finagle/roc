package roc
package postgresql
package server

import cats.std.all._
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.specification.core._
import roc.postgresql.server.ErrorNoticeMessageFields._

/** Used to generate valid and invalid Error / Notice Responses
  *
  * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
  * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]]
  */
trait ErrorNoticeGen extends ScalaCheck {

    lazy val validErrorNoticeTokens: List[Char] = List(Severity, 
      ErrorNoticeMessageFields.Code, Message, Detail, Hint, Position, InternalPosition, 
      InternalQuery, Where, SchemaName, TableName, ColumnName, DataTypeName, ConstraintName, File, 
      Line, Routine)
    lazy val validRequiredErrorNoticeToken: List[Char] = List(Severity,
      ErrorNoticeMessageFields.Code, Message)
    lazy val validOptionalErrorNoticeTokens: List[Char] = List(Detail, Hint, Position, 
      InternalPosition, InternalQuery, Where, SchemaName, TableName, ColumnName, DataTypeName, 
      ConstraintName, File, Line, Routine)
    lazy val genValidErrorNoticeFieldToken: Gen[Char] = Gen.oneOf(validErrorNoticeTokens)
    lazy val genTokenValues: Gen[Fields] = for {
      severity            <-  arbitrary[String]
      code                <-  arbitrary[String]
      message             <-  arbitrary[String]
      detail              <-  arbitrary[String]
      hint                <-  arbitrary[String]
      position            <-  arbitrary[String]
      internalPosition    <-  arbitrary[String]
      internalQuery       <-  arbitrary[String]
      where               <-  arbitrary[String]
      schemaName          <-  arbitrary[String]
      tableName           <-  arbitrary[String]
      columnName          <-  arbitrary[String]
      dataTypeName        <-  arbitrary[String]
      constraintName      <-  arbitrary[String]
      file                <-  arbitrary[String]
      line                <-  arbitrary[String]
      routine             <-  arbitrary[String]
    } yield {
      List((Severity, severity), (ErrorNoticeMessageFields.Code, code), (Message, message),
        (Detail, detail), (Hint, hint), (Position, position), (InternalPosition, internalPosition),
        (InternalQuery, internalQuery), (Where, where), (SchemaName, schemaName),
        (TableName, tableName), (ColumnName, columnName), (DataTypeName, dataTypeName),
        (ConstraintName, constraintName), (File, file), (Line, line), (Routine, routine))
    }
    lazy val genExtractValueByCodeContainer: Gen[ExtractValueByCodeContainer] = for {
      code    <-  genValidErrorNoticeFieldToken
      xs      <-  genTokenValues
    } yield new ExtractValueByCodeContainer(code, xs)
    implicit lazy val arbitraryExtractValueByCodeContainer: Arbitrary[ExtractValueByCodeContainer] =
      Arbitrary(genExtractValueByCodeContainer)

    lazy val genValidSeverityField: Gen[String] = Gen.oneOf("ERROR", "FATAL", "PANIC")
    lazy val genValidSQLSTATECode: Gen[String] = Gen.oneOf(errorClassCodeList)
    lazy val validRequiredFieldsGen: Gen[Fields] = for {
      severity      <-  genValidSeverityField
      sqlStateCode  <-  genValidSQLSTATECode
      message       <-  arbitrary[String]
    } yield List((Severity, severity), (ErrorNoticeMessageFields.Code, sqlStateCode),
      (Message, message))
    lazy val invalidSeverityFieldsGen: Gen[Fields] = for {
      sqlStateCode  <-  genValidSQLSTATECode
      message       <-  arbitrary[String]
    } yield List((ErrorNoticeMessageFields.Code, sqlStateCode), (Message, message))
    lazy val invalidSqlStateCodeFieldsGen: Gen[Fields] = for {
      severity  <-  genValidSeverityField
      message   <-  arbitrary[String]
    } yield List((Severity, severity), (Message, message))
    lazy val invalidMessageFieldsGen: Gen[Fields] = for {
      severity      <-  genValidSeverityField
      sqlStateCode  <-  genValidSQLSTATECode    
    } yield List((Severity, severity), (ErrorNoticeMessageFields.Code, sqlStateCode))
    lazy val invalidSqlStateCodeMessageFieldsGen: Gen[Fields] = for {
      severity  <-  genValidSeverityField
    } yield List((Severity, severity))
    lazy val invalidSeverityMessageFieldsGen: Gen[Fields] = for {
      sqlStateCode  <-  genValidSQLSTATECode
    } yield List((ErrorNoticeMessageFields.Code, sqlStateCode))
    lazy val invalidSeveritySqlStateCodeFieldsGen: Gen[Fields] = for {
      message   <-  arbitrary[String]
    } yield List((Message, message))
    lazy val genOptionalFields: Gen[Fields] = for {
      detail              <-  arbitrary[String]
      hint                <-  arbitrary[String]
      position            <-  arbitrary[String]
      internalPosition    <-  arbitrary[String]
      internalQuery       <-  arbitrary[String]
      where               <-  arbitrary[String]
      schemaName          <-  arbitrary[String]
      tableName           <-  arbitrary[String]
      columnName          <-  arbitrary[String]
      dataTypeName        <-  arbitrary[String]
      constraintName      <-  arbitrary[String]
      file                <-  arbitrary[String]
      line                <-  arbitrary[String]
      routine             <-  arbitrary[String]
    } yield List((Detail, detail), (Hint, hint), (Position, position), (InternalPosition, 
        internalPosition), (InternalQuery, internalQuery), (Where, where), (SchemaName, schemaName),
        (TableName, tableName), (ColumnName, columnName), (DataTypeName, dataTypeName), 
        (ConstraintName, constraintName), (File, file), (Line, line), (Routine, routine))
    lazy val validFieldsGen: Gen[Fields] = for {
      required  <- validRequiredFieldsGen
      optional  <- genOptionalFields
    } yield {
      val filteredOptional = optional.filterNot(x => {x._2 == None || x._2 == Some("")})
      required ::: filteredOptional
    }
    lazy val invalidRequiredFieldsGen: Gen[Seq[Char]] = {
      val r = scala.util.Random
      val count = r.nextInt(validRequiredErrorNoticeToken.length)
      Gen.pick(count, validRequiredErrorNoticeToken)
    }
    lazy val invalidFieldsGen: Gen[Fields] = for {
      required      <-  invalidRequiredFieldsGen
      severity      <-  genValidSeverityField
      sqlStateCode  <-  genValidSQLSTATECode
      message       <-  arbitrary[String]
      optional      <-  genOptionalFields
    } yield {
      val filteredOptional = optional.filterNot(x => {x._2 == None || x._2 == Some("")})
      required.map(c => c match { 
        case Severity                      => (c, severity)
        case ErrorNoticeMessageFields.Code => (c, sqlStateCode)
        case Message                       => (c, message)
      })
      .toList ::: filteredOptional
    }
    lazy val errorParamsGen: Gen[ErrorParams] = for {
      severity      <-  genValidSeverityField
      sqlStateCode  <-  genValidSQLSTATECode
      message       <-  arbitrary[String]
      optional      <-  genOptionalFields
    } yield {
      val xs = optional.filterNot(x => {x._2 == None || x._2 == Some("")})
      val detail           = PostgresqlError.extractValueByCode(Detail, xs)
      val hint             = PostgresqlError.extractValueByCode(Hint, xs)
      val position         = PostgresqlError.extractValueByCode(Position, xs)
      val internalPosition = PostgresqlError.extractValueByCode(InternalPosition, xs)
      val internalQuery    = PostgresqlError.extractValueByCode(InternalQuery, xs)
      val where            = PostgresqlError.extractValueByCode(Where, xs)
      val schemaName       = PostgresqlError.extractValueByCode(SchemaName, xs)
      val tableName        = PostgresqlError.extractValueByCode(TableName, xs)
      val columnName       = PostgresqlError.extractValueByCode(ColumnName, xs)
      val dataTypeName     = PostgresqlError.extractValueByCode(DataTypeName, xs)
      val constraintName   = PostgresqlError.extractValueByCode(ConstraintName, xs)
      val file             = PostgresqlError.extractValueByCode(File, xs)
      val line             = PostgresqlError.extractValueByCode(Line, xs)
      val routine          = PostgresqlError.extractValueByCode(Routine, xs)
      new ErrorParams(severity = severity, code = sqlStateCode, message = message, 
        detail = detail, hint = hint, position = position, internalPosition = internalPosition, 
        internalQuery = internalQuery, where = where, schemaName = schemaName,
        tableName = tableName, columnName = columnName, dataTypeName = dataTypeName, 
        constraintName = constraintName, file = file, line = line, routine = routine)
    }
    implicit lazy val arbitraryErrorParams: Arbitrary[ErrorParams] = Arbitrary(errorParamsGen)

    protected def buildFields(severity: String, code: String, message: String, 
      optional: List[(Char, String)]): Fields = {
        val xs = optional.filterNot(x => {x._2 == None || x._2 == Some("")})
        val ys = List((Severity, severity), (ErrorNoticeMessageFields.Code, code),
          (Message, message))
        ys ::: xs
      }

    protected def buildErrorParamsFromFields(xs: Fields): ErrorParams = {
      val severity = xs.find(_._1 === Severity).map(_._2)
        .getOrElse(throw new Exception("Severity must be generated"))
      val code = xs.find(_._1 === ErrorNoticeMessageFields.Code).map(_._2)
        .getOrElse(throw new Exception("SQLSTATE Code must be generated"))
      val message = xs.find(_._1 === Message).map(_._2)
        .getOrElse(throw new Exception("Message must be generated"))
      val detail           = PostgresqlError.extractValueByCode(Detail, xs)
      val hint             = PostgresqlError.extractValueByCode(Hint, xs)
      val position         = PostgresqlError.extractValueByCode(Position, xs)
      val internalPosition = PostgresqlError.extractValueByCode(InternalPosition, xs)
      val internalQuery    = PostgresqlError.extractValueByCode(InternalQuery, xs)
      val where            = PostgresqlError.extractValueByCode(Where, xs)
      val schemaName       = PostgresqlError.extractValueByCode(SchemaName, xs)
      val tableName        = PostgresqlError.extractValueByCode(TableName, xs)
      val columnName       = PostgresqlError.extractValueByCode(ColumnName, xs)
      val dataTypeName     = PostgresqlError.extractValueByCode(DataTypeName, xs)
      val constraintName   = PostgresqlError.extractValueByCode(ConstraintName, xs)
      val file             = PostgresqlError.extractValueByCode(File, xs)
      val line             = PostgresqlError.extractValueByCode(Line, xs)
      val routine          = PostgresqlError.extractValueByCode(Routine, xs)
      new ErrorParams(severity = severity, code = code, message = message, 
        detail = detail, hint = hint, position = position, internalPosition = internalPosition, 
        internalQuery = internalQuery, where = where, schemaName = schemaName,
        tableName = tableName, columnName = columnName, dataTypeName = dataTypeName, 
        constraintName = constraintName, file = file, line = line, routine = routine)
    }
    
    import ErrorClassCodes._
    protected val errorClassCodeList = List(SuccessfulCompletion, Warning, NoData, 
      SQLStatementNotYetComplete, ConnectionException, TriggeredActionException,
      FeatureNotSupported, InvalidTransactionInitiation, LocatorException, InvalidGrantor,
      InvalidRoleSpecification, DiagnosisException, CaseNotFound, CardinalityViolation, 
      DataException, IntegrityConstraintViolation, InvalidCursorState, InvalidTransactionState,
      InvalidSQLStatementName, TriggeredDataChangeViolation, InvalidAuthorizationSpecification,
      DependentPrivilegeDescriptorsStillExist, InvalidTransactionTermination, SQLRoutineException,
      InvalidCursorName, ExternalRoutineException, ExternalRoutineInvocationException,
      SavepointException, InvalidCatalogName, InvalidSchemaName, TransactionRollback,
      SyntaxErrorOrAccessRuleViolation, WithCheckOptionViolation, InsufficientResources,
      ProgramLimitExceeded, ObjectNotInPrerequisiteState, OperatorIntervention, SystemError,
      ConfigurationFileError, ForeignDataWrapperError, PLpgSQLError, InternalError)

    lazy val unknownErrorCodeGen: Gen[String] =
      arbitrary[String] suchThat(x => !errorClassCodeList.contains(x))

    lazy val unknownErrorGen: Gen[FieldsAndErrorParams] = for {
      severity      <-  genValidSeverityField
      sqlStateCode  <-  unknownErrorCodeGen
      message       <-  arbitrary[String]
      optional      <-  genOptionalFields
    } yield {
      val fields = buildFields(severity, sqlStateCode, message, optional)
      val e = buildErrorParamsFromFields(fields)
      new FieldsAndErrorParams(fields, e)
    }

    case class ExtractValueByCodeContainer(code: Char, xs: Fields)

    case class FieldsAndErrorParams(fields: Fields, errorParams: ErrorParams)
}
