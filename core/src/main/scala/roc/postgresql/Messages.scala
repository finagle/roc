package roc
package postgresql

import algebra.Eq
import cats.data.Xor
import cats.std.all._
import cats.syntax.eq._
import com.twitter.util.Future
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import roc.postgresql.server.PostgresqlError
import roc.postgresql.transport.{Buffer, BufferReader, BufferWriter, Packet}
import scala.collection.mutable.ListBuffer

sealed trait Message 
object Message {
  val AuthenticationMessageByte: Char = 'R'
  val ErrorByte: Char                 = 'E'
  val ParameterStatusByte: Char       = 'S'
  val ReadyForQueryByte: Char         = 'Z'
  val BackendKeyDataByte: Char        = 'K'
  val RowDescriptionByte: Char        = 'T'
  val DataRowByte: Char               = 'D'
  val CommandCompleteByte: Char       = 'C'
  val PasswordMessageByte: Char       = 'p'
  val QueryMessageByte: Char          = 'Q'
  val EmptyQueryResponseByte: Char    = 'I'
  val TerminateByte: Char             = 'X'
  val NoticeResponseByte: Char        = 'N'

  private[roc] def decode(packet: Packet): Xor[Failure, Message] = packet.messageType match {
    case Some(mt) if mt === AuthenticationMessageByte => decodePacket[AuthenticationMessage](packet)
    case Some(mt) if mt === ErrorByte => decodePacket[ErrorResponse](packet)
    case Some(mt) if mt === ParameterStatusByte => decodePacket[ParameterStatus](packet)
    case Some(mt) if mt === BackendKeyDataByte => decodePacket[BackendKeyData](packet)
    case Some(mt) if mt === ReadyForQueryByte => decodePacket[ReadyForQuery](packet)
    case Some(mt) if mt === RowDescriptionByte => decodePacket[RowDescription](packet)
    case Some(mt) if mt === DataRowByte => decodePacket[DataRow](packet)
    case Some(mt) if mt === CommandCompleteByte => decodePacket[CommandComplete](packet)
    case Some(mt) if mt === EmptyQueryResponseByte => Xor.Right(EmptyQueryResponse)
    case Some(mt) => {
        println(s"Inside Some($mt)")
        Xor.Left(new UnknownPostgresqlMessageTypeFailure(mt))
    }
    case None => Xor.Left(new UnexpectedNoneFailure(""))
  }

  implicit val messageEq: Eq[Message] = new Eq[Message] {
    def eqv(x: Message, y: Message): Boolean = x == y
  }
}

sealed trait FrontendMessage extends Message
sealed trait Transmission

case class StartupMessage(user: String, database: String) extends FrontendMessage
case class Query(queryString: String) extends FrontendMessage with Transmission

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

final class Terminate extends FrontendMessage

sealed trait BackendMessage extends Message

case class ErrorResponse(error: PostgresqlError) extends BackendMessage

sealed trait AuthenticationMessage extends BackendMessage
object AuthenticationMessage {
  def apply(tuple: (Int, Option[Array[Byte]])): Failure Xor AuthenticationMessage = tuple match {
    case (0, None)        => Xor.Right(AuthenticationOk)
    case (2, None)        => Xor.Right(AuthenticationKerberosV5)
    case (3, None)        => Xor.Right(AuthenticationClearTxtPasswd)
    case (5, Some(bytes)) => Xor.Right(new AuthenticationMD5Passwd(bytes))
    case (6, None)        => Xor.Right(AuthenticationSCMCredential)
    case (7, None)        => Xor.Right(AuthenticationGSS)
    case (8, Some(bytes)) => Xor.Right(new AuthenticationGSSContinue(bytes))
    case (9, None)        => Xor.Right(AuthenticationSSPI)
    case (x, _)           => Xor.Left(new UnknownAuthenticationRequestFailure(x))
  }
}
case object AuthenticationOk extends AuthenticationMessage
case object AuthenticationClearTxtPasswd extends AuthenticationMessage
case class AuthenticationMD5Passwd(salt: Array[Byte]) extends AuthenticationMessage {
  def canEqual(a: Any) = a.isInstanceOf[AuthenticationMD5Passwd]

  final override def equals(that: Any): Boolean = that match {
    case x: AuthenticationMD5Passwd => x.canEqual(this) && salt.length == x.salt.length &&
      (salt sameElements x.salt)
    case _ => false
  }
}

case object AuthenticationKerberosV5 extends AuthenticationMessage
case object AuthenticationSCMCredential extends AuthenticationMessage
case object AuthenticationGSS extends AuthenticationMessage
case object AuthenticationSSPI extends AuthenticationMessage
case class AuthenticationGSSContinue(authBytes: Array[Byte]) extends AuthenticationMessage {
  def canEqual(a: Any) = a.isInstanceOf[AuthenticationGSSContinue]

  final override def equals(that: Any): Boolean = that match {
    case x: AuthenticationGSSContinue => x.canEqual(this) && 
      authBytes.length == x.authBytes.length &&
      (authBytes sameElements x.authBytes)
    case _ => false
  }
}

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

case object EmptyQueryResponse extends BackendMessage

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

case class NoticeResponse(byte: Char, reason: String) extends BackendMessage
