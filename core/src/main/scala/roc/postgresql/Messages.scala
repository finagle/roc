package com.github.finagle
package roc
package postgresql

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
  val ErrorMessage: Char          = 'E'
  val ParameterStatus: Char       = 'S'
  val ReadyForQuery: Char         = 'Z'
  val BackendKeyData: Char        = 'K'
  val RowDescription: Char        = 'T'
  val DataRow: Char               = 'D'
  val CommandComplete: Char       = 'C'

  private[postgresql] def lengthOfCStyleString(str: String): Int = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    bytes.length + 1
  }
}

sealed trait FrontendMessage extends Message {
  def encode: Packet
}
sealed trait BackendMessage extends Message

case class ErrorMessage(byte: Char, reason: String) extends Message
object ErrorMessage {

  def apply(packet: Packet): Future[ErrorMessage] = {
    val br       = BufferReader(packet.body)
    val byte     = br.readByte
    val response = br.readNullTerminatedString()
    Future.value(new ErrorMessage(byte.toChar, response))
  }
}

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

case class ParameterStatusMessage(parameter: String, value: String) extends BackendMessage
object ParameterStatusMessage {
  def apply(packet: Packet): Future[ParameterStatusMessage] = {
    val br = BufferReader(packet.body)
    val param = br.readNullTerminatedString()
    val value = br.readNullTerminatedString()
    Future.value(new ParameterStatusMessage(param, value))
  }
}

case class BackendKeyData(processId: Int, secretKey: Int) extends BackendMessage
object BackendKeyData {
  def apply(packet: Packet): Future[BackendKeyData] = {
    val br = BufferReader(packet.body)
    val procId = br.readInt
    val secretKey = br.readInt
    Future.value(new BackendKeyData(procId, secretKey))
  }
}

case class ReadyForQuery(transactionStatus: Char) extends BackendMessage
object ReadyForQuery {
  def apply(packet: Packet): Future[ReadyForQuery] = {
    val br = BufferReader(packet.body)
    val tStatus = br.readByte
    val rfq = new ReadyForQuery(tStatus.toChar)
    Future.value(rfq)
  }
}

case class StartupMessage(user: String, database: String) extends FrontendMessage {
  import StartupMessage._

  def encode: Packet = {
    val buffer = BufferWriter(new Array[Byte](lengthOfByteArray(this)))
    buffer.writeShort(3)
    buffer.writeShort(0)
    buffer.writeNullTerminatedString("user")
    buffer.writeNullTerminatedString(user)
    buffer.writeNullTerminatedString("database")
    buffer.writeNullTerminatedString(database)
    buffer.writeNull
    Packet(None, Buffer(buffer.toBytes))
  }
}
object StartupMessage {
  private[postgresql] def lengthOfByteArray(sm: StartupMessage): Int = {
    val protocolLength  = 4 //2 shorts * 2
    val lengthOfUserLbl = Message.lengthOfCStyleString("user")
    val lengthOfUser    = Message.lengthOfCStyleString(sm.user)
    val lengthOfDbLbl   = Message.lengthOfCStyleString("database")
    val lengthOfDb      = Message.lengthOfCStyleString(sm.database)
    val extraNull       = 1

    protocolLength + lengthOfUserLbl + lengthOfUser + lengthOfDbLbl + lengthOfDb + extraNull
  }
}

case class Query(queryString: String) extends FrontendMessage {
  def encode: Packet = {
    val length = queryString.getBytes.length
    val bw = BufferWriter(new Array[Byte](length + 1))
    bw.writeNullTerminatedString(queryString)
    val bytes = bw.toBytes
    Packet(Some('Q'), Buffer(bytes))
  }
}

case class RowDescription(numFields: Short, fields: List[RowDescriptionField]) extends Message 
object RowDescription {
  def apply(packet: Packet): Future[RowDescription] = {
    val br = BufferReader(packet.body)
    val numFields = br.readShort

    @annotation.tailrec
    def loop(currCount: Short, fs: List[RowDescriptionField]): List[RowDescriptionField] =
      currCount match {
        case x if x < numFields => {
          val name = br.readNullTerminatedString()
          val tableObjectId = br.readInt
          val tableAttributeId = br.readShort 
          val dataTypeObjectId = br.readInt
          val dataTypeSize = br.readShort
          val typeModifier = br.readInt
          val formatCode = br.readShort match {
            case 0 => Text
            case 1 => Binary
            case _ => throw new Exception()
          }

          val rdf = RowDescriptionField(name, tableObjectId, tableAttributeId, dataTypeObjectId,
            dataTypeSize, typeModifier, formatCode)
          loop((currCount + 1).toShort, rdf :: fs)
        }
        case x if x >= numFields => fs
      }

    val fs = loop(0, List.empty[RowDescriptionField]).reverse
    Future.value(RowDescription(numFields, fs))
  }
}
case class RowDescriptionField(name: String, tableObjectId: Int, tableAttributeId: Short,
  dataTypeObjectId: Int, dataTypeSize: Short, typeModifier: Int, formatCode: FormatCode)

case class PasswordMessage(password: String) extends FrontendMessage {
  def encode: Packet = {
    val length = password.getBytes(StandardCharsets.UTF_8).length
    val bw = BufferWriter(new Array[Byte](length + 1))
    bw.writeNullTerminatedString(password)
    Packet(Some('p'), Buffer(bw.toBytes))
  }
}
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

case class DataRow(numColumns: Short, columnBytes: List[Option[Array[Byte]]]) extends BackendMessage 
object DataRow {
  def apply(packet: Packet): Future[DataRow] = {
    val br = BufferReader(packet.body)
    val columns = br.readShort

    @annotation.tailrec
    def loop(idx: Short, cbs: ListBuffer[Option[Array[Byte]]]): List[Option[Array[Byte]]] = 
      idx match {
        case x if x < columns => {
          val columnLength = br.readInt
          val bytes = if(columnLength == -1) {
            None
          } else if(columnLength == 0) {
            Some(Array.empty[Byte])
          } else {
            Some(br.take(columnLength))
          }
          loop((idx + 1).toShort, cbs += bytes)
        }
        case x if x >= columns => cbs.toList
      }

    val columnBytes = loop(0, ListBuffer.empty[Option[Array[Byte]]])
    Future.value(new DataRow(columns, columnBytes))
  }
}

case class CommandComplete(commandTag: String) extends BackendMessage
object CommandComplete {
  def apply(packet: Packet): Future[CommandComplete] = {
    val br = BufferReader(packet.body)
    val commandTag = br.readNullTerminatedString()

    Future.value(new CommandComplete(commandTag))
  }
}
