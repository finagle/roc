package com.github.finagle
package roc
package postgresql

import cats.data.Xor
import cats.std.all._
import cats.syntax.eq._
import com.github.finagle.roc.postgresql.transport.{Buffer, BufferReader, BufferWriter, Packet}
import com.twitter.util.Future
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.mutable.ListBuffer

sealed trait Message
object Message {
  val AuthenticationRequest: Char = 'R'
  val ErrorByte: Char             = 'E'
  val ParameterStatusByte: Char   = 'S'
  val ReadyForQueryByte: Char     = 'Z'
  val BackendKeyDataByte: Char    = 'K'
  val RowDescriptionByte: Char    = 'T'
  val DataRowByte: Char           = 'D'
  val CommandCompleteByte: Char   = 'C'
  val PasswordMessageByte: Char   = 'p'
  val QueryMessageByte: Char      = 'Q'
}

sealed trait FrontendMessage extends Message

case class StartupMessage(user: String, database: String) extends FrontendMessage
case class Query(queryString: String) extends FrontendMessage

case class PasswordMessage(password: String) extends FrontendMessage
object PasswordMessage {
  private[postgresql] def encryptMD5Passwd(user: String, passwd: String, 
    salt: Array[Byte]): String = {
      val md = MessageDigest.getInstance("MD5")
      md.update((passwd + user).getBytes)
      val unsaltedHexStr = md.digest().map(x => "%02x".format(x.byteValue)).foldLeft("")(_ + _)
      val saltedBytes = unsaltedHexStr.getBytes ++ salt
      md.reset()
      md.update(saltedBytes)
      md.digest().map(x => "%02x".format(x.byteValue)).foldLeft("md5")(_ + _)
    }
}


sealed trait BackendMessage extends Message

case class ErrorMessage(byte: Char, reason: String) extends BackendMessage

sealed trait AuthenticationMessage extends BackendMessage
object AuthenticationMessage {

  def apply(packet: Packet): Future[AuthenticationMessage] = {
    val br = BufferReader(packet.body)
    br.readInt match {
      case 0 => Future.value(AuthenticationOk)
      case 3 => Future.value(AuthenticationClearTxtPasswd)
      case 5 => {
        val salt = br.take(4)
        Future.value(new AuthenticationMD5Passwd(salt))
      }
      case x => Future.exception(new InvalidAuthenticationRequest(x))
    }
  }
}
case object AuthenticationOk extends AuthenticationMessage
case object AuthenticationClearTxtPasswd extends AuthenticationMessage
case class AuthenticationMD5Passwd(salt: Array[Byte]) extends AuthenticationMessage

case class ParameterStatus(parameter: String, value: String) extends BackendMessage
case class BackendKeyData(processId: Int, secretKey: Int) extends BackendMessage

sealed trait ReadyForQuery extends BackendMessage
object ReadyForQuery {
  def apply(transactionStatus: Char): ReadyForQueryDecodingFailure Xor ReadyForQuery = 
    transactionStatus match {
      case 'I' => Xor.Right(Idle)
      case 'T' => Xor.Right(TransactionBlock)
      case 'E' => Xor.Right(FailedTransactionBlock)
      case  c  => Xor.Left(new ReadyForQueryDecodingFailure(c))
    }
}

case object Idle extends ReadyForQuery
case object TransactionBlock extends ReadyForQuery
case object FailedTransactionBlock extends ReadyForQuery

case class RowDescription(numFields: Short, fields: List[RowDescriptionField]) extends BackendMessage 
case class RowDescriptionField(name: String, tableObjectId: Int, tableAttributeId: Short,
  dataTypeObjectId: Int, dataTypeSize: Short, typeModifier: Int, formatCode: FormatCode)

sealed trait FormatCode
case object Text extends FormatCode
case object Binary extends FormatCode

case class DataRow(numColumns: Short, columnBytes: List[Option[Array[Byte]]]) extends BackendMessage {

  def canEqual(a: Any) = a.isInstanceOf[DataRow]

  final override def equals(that: Any): Boolean = that match {
    case x: DataRow => {
      val equalColumnBytes = (columnBytes.length == x.columnBytes.length &&
        columnBytes.zip(x.columnBytes).forall(y => y._1 match {
          case None          => y._1 == y._2
          case Some(leftArr) => y._2 match {
            case None           => false
            case Some(rightArr) => leftArr sameElements rightArr
          }
        }))
      x.canEqual(this) && numColumns == x.numColumns && equalColumnBytes
    }
    case _ => false
  }
}
case class CommandComplete(commandTag: String) extends BackendMessage
