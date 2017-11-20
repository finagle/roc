package roc
package postgresql

import cats.implicits._
import cats.kernel.Eq
import java.security.MessageDigest
import roc.postgresql.failures.{Failure, ReadyForQueryDecodingFailure, UnexpectedNoneFailure,
  UnknownAuthenticationRequestFailure, UnknownPostgresqlMessageTypeFailure}
import roc.postgresql.server.PostgresqlMessage
import roc.postgresql.transport.Packet

private[postgresql] sealed abstract class Message
private[postgresql] object Message {
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

  private[postgresql] def decode(packet: Packet): Either[Failure, Message] = packet.messageType match {
    case Some(mt) if mt === AuthenticationMessageByte => decodePacket[AuthenticationMessage](packet)
    case Some(mt) if mt === ErrorByte => decodePacket[ErrorResponse](packet)
    case Some(mt) if mt === NoticeResponseByte => decodePacket[NoticeResponse](packet)
    case Some(mt) if mt === ParameterStatusByte => decodePacket[ParameterStatus](packet)
    case Some(mt) if mt === BackendKeyDataByte => decodePacket[BackendKeyData](packet)
    case Some(mt) if mt === ReadyForQueryByte => decodePacket[ReadyForQuery](packet)
    case Some(mt) if mt === RowDescriptionByte => decodePacket[RowDescription](packet)
    case Some(mt) if mt === DataRowByte => decodePacket[DataRow](packet)
    case Some(mt) if mt === CommandCompleteByte => decodePacket[CommandComplete](packet)
    case Some(mt) if mt === EmptyQueryResponseByte => Right(EmptyQueryResponse)
    case Some(mt) => Left(new UnknownPostgresqlMessageTypeFailure(mt))
    case None => Left(new UnexpectedNoneFailure(""))
  }

  implicit val messageEq: Eq[Message] = new Eq[Message] {
    def eqv(x: Message, y: Message): Boolean = x == y
  }
}

private[postgresql] sealed trait FrontendMessage extends Message
private[postgresql] sealed trait Transmission

private[postgresql] case class StartupMessage(user: String, database: String) extends FrontendMessage
private[postgresql] case class Query(queryString: String) extends FrontendMessage with Transmission

private[postgresql] case class PasswordMessage(password: String) extends FrontendMessage
private[postgresql] object PasswordMessage {
  def encryptMD5Passwd(user: String, passwd: String, 
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

private[postgresql] final class Terminate extends FrontendMessage

private[postgresql] sealed abstract class BackendMessage extends Message

private[postgresql] case class ErrorResponse(error: PostgresqlMessage) extends BackendMessage

private[postgresql] sealed abstract class AuthenticationMessage extends BackendMessage
private[postgresql] object AuthenticationMessage {
  def apply(tuple: (Int, Option[Array[Byte]])): Failure Either AuthenticationMessage = tuple match {
    case (0, None)        => Right(AuthenticationOk)
    case (2, None)        => Right(AuthenticationKerberosV5)
    case (3, None)        => Right(AuthenticationClearTxtPasswd)
    case (5, Some(bytes)) => Right(new AuthenticationMD5Passwd(bytes))
    case (6, None)        => Right(AuthenticationSCMCredential)
    case (7, None)        => Right(AuthenticationGSS)
    case (8, Some(bytes)) => Right(new AuthenticationGSSContinue(bytes))
    case (9, None)        => Right(AuthenticationSSPI)
    case (x, _)           => Left(new UnknownAuthenticationRequestFailure(x))
  }
}
private[postgresql] case object AuthenticationOk extends AuthenticationMessage
private[postgresql] case object AuthenticationClearTxtPasswd extends AuthenticationMessage
private[postgresql] case class AuthenticationMD5Passwd(salt: Array[Byte])
  extends AuthenticationMessage {
  def canEqual(a: Any) = a.isInstanceOf[AuthenticationMD5Passwd]

  final override def equals(that: Any): Boolean = that match {
    case x: AuthenticationMD5Passwd => x.canEqual(this) && salt.length == x.salt.length &&
      (salt sameElements x.salt)
    case _ => false
  }
}

private[postgresql] case object AuthenticationKerberosV5 extends AuthenticationMessage
private[postgresql] case object AuthenticationSCMCredential extends AuthenticationMessage
private[postgresql] case object AuthenticationGSS extends AuthenticationMessage
private[postgresql] case object AuthenticationSSPI extends AuthenticationMessage
private[postgresql] case class AuthenticationGSSContinue(authBytes: Array[Byte]) 
  extends AuthenticationMessage {
  def canEqual(a: Any) = a.isInstanceOf[AuthenticationGSSContinue]

  final override def equals(that: Any): Boolean = that match {
    case x: AuthenticationGSSContinue => x.canEqual(this) && 
      authBytes.length == x.authBytes.length &&
      (authBytes sameElements x.authBytes)
    case _ => false
  }
}

private[postgresql] case class ParameterStatus(parameter: String, value: String) 
  extends BackendMessage
private[postgresql] case class BackendKeyData(processId: Int, secretKey: Int) extends BackendMessage

private[postgresql] sealed abstract class ReadyForQuery extends BackendMessage
private[postgresql] object ReadyForQuery {
  def apply(transactionStatus: Char): ReadyForQueryDecodingFailure Either ReadyForQuery = 
    transactionStatus match {
      case 'I' => Right(Idle)
      case 'T' => Right(TransactionBlock)
      case 'E' => Right(FailedTransactionBlock)
      case  c  => Left(new ReadyForQueryDecodingFailure(c))
    }
}

private[postgresql] case object Idle extends ReadyForQuery
private[postgresql] case object TransactionBlock extends ReadyForQuery
private[postgresql] case object FailedTransactionBlock extends ReadyForQuery

private[postgresql] case object EmptyQueryResponse extends BackendMessage

private[postgresql] case class RowDescription(numFields: Short, fields: List[RowDescriptionField])
  extends BackendMessage 
private[postgresql] case class RowDescriptionField(name: String, tableObjectId: Int, 
  tableAttributeId: Short, dataTypeObjectId: Int, dataTypeSize: Short, typeModifier: Int,
  formatCode: FormatCode)

private[postgresql] case class DataRow(numColumns: Short, columnBytes: List[Option[Array[Byte]]])
  extends BackendMessage {

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
private[postgresql] case class CommandComplete(commandTag: String) extends BackendMessage

private[postgresql] case class NoticeResponse(message: PostgresqlMessage) extends BackendMessage
