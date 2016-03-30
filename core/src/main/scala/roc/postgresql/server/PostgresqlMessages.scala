package roc
package postgresql
package server

import cats.data.Validated._
import cats.data.{NonEmptyList, Validated, ValidatedNel, Xor}
import cats.Semigroup
import cats.std.all._
import cats.syntax.eq._
import roc.postgresql.failures.{PostgresqlMessageDecodingFailure, Failure}

/** Represents an error that occured on the Postgresql Server.
  *
  * The value members here are possible fields that can be appear in an `ErrorResponse`
  * or `NoticeResponse`. `severity`, `code`, `detail` are always present.
  * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
  * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]]
  */
sealed abstract class PostgresqlMessage private[server](params: ErrorParams) {

  /** The severity of the Error or Notice
    *
    * The field contents are ERROR, FATAL, or PANIC (in an error message), or WARNING, NOTICE, 
    * DEBUG, INFO, or LOG (in a notice message), or a localized translation of one of these.
    * @note Always present.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]]
    */
  val severity: String = params.severity

  /** The SQLSTATE code for the error.
    * @note Not localizable. Always present.
    * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]]
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val code: String = params.code

  /** The primary human-readable error message.
    *
    * This should be accurate but terse (typically one line).
    * @note Always present.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val message: String = params.message

  /** A secondary error message carrying more detail about the problem.
    *
    * It is possible for this message to run multiple lines.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val detail: Option[String] = params.detail

  /** A optional suggestion what to do about the problem.
    *
    * This is intended to differ from Detail in that it offers advice 
    * (potentially inappropriate) rather than hard facts. Might run to multiple lines.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val hint: Option[String] = params.hint

  /** Indicates an error cursor position as an index into the original query string.
    *
    * The field value is a decimal ASCII integer, indicating an error cursor position 
    * as an index into the original query string. The first character has index 1, and 
    * positions are measured in characters not bytes.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val position: Option[String] = params.position

  /** Indicates an error cursor postion as an index of an internally generated command.
    *
    * This is defined the same as [[position]], but it is used when the cursor position refers 
    * to an internally generated command rather than the one submitted by the client. The
    * [[internalQuery]] field will always appear when this field appears.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val internalPosition: Option[String] = params.internalPosition

  /** The text of a failed internally-generated command.
    *
    * This could be, for example, a SQL query issued by a PL/pgSQL function.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val internalQuery: Option[String] = params.internalQuery

  /** An indication of the context in which the error occurred.
    *
    * Presently this includes a call stack traceback of active procedural language functions 
    * and internally-generated queries. The trace is one entry per line, most recent first.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val where: Option[String] = params.where

  /** The name of the schema containing that object.
    *
    * If the error was associated with a specific database object, the name of the schema 
    * containing that object, if any.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val schemaName: Option[String] = params.schemaName

  /** The name of the table.
    *
    * If the error was associated with a specific table, the name of the table. 
    * (Refer to the schema name field for the name of the table's schema.)
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val tableName: Option[String] = params.tableName

  /** The name of the column.
    *
    * If the error was associated with a specific table column, the name of the column. 
    * (Refer to the schema and table name fields to identify the table.)
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val columnName: Option[String] = params.columnName

  /** The name of the data type.
    *
    * If the error was associated with a specific data type, the name of the data type. 
    * (Refer to the schema name field for the name of the data type's schema.)
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val dataTypeName: Option[String] = params.dataTypeName

  /** The name of the constraint.
    *
    * If the error was associated with a specific constraint, the name of the constraint. 
    * Refer to fields listed above for the associated table or domain. (For this purpose, indexes 
    * are treated as constraints, even if they weren't created with constraint syntax.)
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val constraintName: Option[String] = params.constraintName

  /** The file name of the source-code location where the error was reported.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val file: Option[String] = params.file

  /** The line number of the source-code location where the error was reported.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val line: Option[String] = params.line

  /** The name of the source-code routine reporting the error.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-error-fields.html]]
    */
  val routine: Option[String] = params.routine
}

private[postgresql] case class ErrorParams(severity: String, code: String, message: String, 
  detail: Option[String], hint: Option[String], position: Option[String],
  internalPosition: Option[String], internalQuery: Option[String], where: Option[String], 
  schemaName: Option[String], tableName: Option[String], columnName: Option[String],
  dataTypeName: Option[String], constraintName: Option[String], file: Option[String], 
  line: Option[String], routine: Option[String])

private[postgresql] case class RequiredParams(severity: String, code: String, message: String)

private[postgresql] object PostgresqlMessage {
  import ErrorNoticeMessageFields._

  def apply(xs: Fields): Xor[Failure, PostgresqlMessage] =
    buildParamsFromTuples(xs).flatMap(x => x.code.take(2) match {
      case ErrorClassCodes.SuccessfulCompletion => Xor.Right(new SuccessMessage(x))
      case code if ErrorClassCodes.WarningCodes.contains(code) => Xor.Right(new WarningMessage(x))
      case code if ErrorClassCodes.ErrorCodes.contains(code) => Xor.Right(new ErrorMessage(x))
      case code => Xor.Right(new UnknownMessage(x))
    })

  // private to server for testing
  private[server] def buildParamsFromTuples(xs: List[Field]): 
    Xor[PostgresqlMessageDecodingFailure, ErrorParams] = {
      val detail           = extractValueByCode(Detail, xs)
      val hint             = extractValueByCode(Hint, xs)
      val position         = extractValueByCode(Position, xs)
      val internalPosition = extractValueByCode(InternalPosition, xs)
      val internalQuery    = extractValueByCode(InternalQuery, xs)
      val where            = extractValueByCode(Where, xs)
      val schemaName       = extractValueByCode(SchemaName, xs)
      val tableName        = extractValueByCode(TableName, xs)
      val columnName       = extractValueByCode(ColumnName, xs)
      val dataTypeName     = extractValueByCode(DataTypeName, xs)
      val constraintName   = extractValueByCode(ConstraintName, xs)
      val file             = extractValueByCode(File, xs)
      val line             = extractValueByCode(Line, xs)
      val routine          = extractValueByCode(Routine, xs)

      val severity = xs.find(_._1 === Severity) match {
        case Some(x) => Valid(x._2)
        case None    => Invalid("Required Severity Level was not present.")
      }
      val code = xs.find(_._1 === Code) match {
        case Some(x) => Valid(x._2)
        case None    => Invalid("Required SQLSTATE Code was not present.")
      }
      val message = xs.find(_._1 === Message) match {
        case Some(x) => Valid(x._2)
        case None    => Invalid("Required Message was not present.")
      }

      validatePacket(severity.toValidatedNel, code.toValidatedNel, 
        message.toValidatedNel)(RequiredParams.apply)
        .fold(
          {l => Xor.Left(new PostgresqlMessageDecodingFailure(l))},
          {r => Xor.Right(new ErrorParams(severity = r.severity, code = r.code, message = r.message,
            detail = detail, hint = hint, position = position, internalPosition = internalPosition,
            internalQuery = internalQuery, where = where, schemaName = schemaName,
            tableName = tableName, columnName = columnName, dataTypeName = dataTypeName, 
            constraintName = constraintName, file = file, line = line, routine = routine))}
        )
    }

  // private to server for testing
  private[server] def extractValueByCode(code: Char, xs: Fields): Option[String] =
    xs.find(_._1 === code).map(_._2)

  //private to server for testing
  private[server] def validatePacket[E : Semigroup, A, B, C, D]
      (v1: Validated[E, A], v2: Validated[E, B], v3: Validated[E, C])
      (f: (A, B, C) => D): Validated[E, D] = (v1, v2, v3) match {
        case (Valid(a), Valid(b), Valid(c))          => Valid(f(a,b,c))
        case (i@Invalid(_), Valid(_), Valid(_))      => i
        case (Valid(_), i@Invalid(_), Valid(_))      => i
        case (Valid(_), Valid(_), i@Invalid(_))      => i
        case (Valid(_), Invalid(e1), Invalid(e2))    => Invalid(Semigroup[E].combine(e1, e2))
        case (Invalid(e1), Valid(_), Invalid(e2))    => Invalid(Semigroup[E].combine(e1, e2))
        case (Invalid(e1), Invalid(e2), Valid(_))    => Invalid(Semigroup[E].combine(e1, e2))
        case (Invalid(e1), Invalid(e2), Invalid(e3)) => 
          Invalid(Semigroup[E].combine(e1, Semigroup[E].combine(e2, e3)))
      }
}

/** Represents an unknown or undefined message.
  *
  * From Postgresql Documentation: "Since more field types might be added in future, 
  * frontends should silently ignore fields of unrecognized type." Therefore, if we decode
  * an Error we do not recognize, we do not create a Failed Decoding Result.
  */
final case class UnknownMessage private[server](params: ErrorParams)
  extends PostgresqlMessage(params)


/** Represents a set of Successful Message
  *
  * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]]
  */
final case class SuccessMessage private[server](params: ErrorParams)
  extends PostgresqlMessage(params)


/** Represents a set of Warning Messages
  *
  * ==Warning Messages==
  *
  *  1. warning
  *  1. dynamic_result_sets_returned
  *  1. implicit_zero_bit_padding
  *  1. null_value_eliminated_in_set_function
  *  1. privilege_not_granted
  *  1. privilege_not_revoked
  *  1. string_data_right_truncation
  *  1. deprecated_feature
  *  1. no_data
  *  1. no_additional_dynamic_result_sets_returned
  *
  * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]]
  * @see [[https://github.com/postgres/postgres/blob/master/src/backend/utils/errcodes.txt]]
  */
final case class WarningMessage private[server](private val params: ErrorParams)
  extends PostgresqlMessage(params)

/** Represents a set of Error Messages
  *
  * @see [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html]]
  * @see [[https://github.com/postgres/postgres/blob/master/src/backend/utils/errcodes.txt]]
  */
final case class ErrorMessage private[server](private val params: ErrorParams)
  extends PostgresqlMessage(params) {
  override def toString: String = s"$severity - $message. SQLSTATE: $code."
}
