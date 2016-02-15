package com.github.finagle
package roc
package postgresql

import com.twitter.util.Future
import com.github.finagle.roc.postgresql.transport.{Buffer, BufferReader, BufferWriter, Packet}
import cats.syntax.eq._
import cats.std.all._

sealed trait Message {
  def toPacket: Packet = Packet(None, BufferWriter(Array[Byte](0)))
}

object Message {
  val AuthenticationRequest: Byte = 0x52 // 'R'
  val ErrorMessage: Byte          = 0x45 // 'E'
  val ParameterStatus: Char       = 'S'
  val ReadyForQuery: Char         = 'Z'
  val BackendKeyData: Char        = 'K'
  val RowDescription: Char        = 'T'
}

case class ErrorMessage(byte: Char, reason: String) extends Message
object ErrorMessage {

  def apply(packet: Packet): Future[ErrorMessage] = {
    val br       = BufferReader(packet.body)
    val byte     = br.readByte
    val response = br.readNullTerminatedString()
    Future.value(new ErrorMessage(byte.toChar, response))
  }
}

sealed trait StartupMessages extends Message

case class StartupMessage(user: String, database: String) extends StartupMessages {

  override def toPacket: Packet = {
    val buffer = BufferWriter(new Array[Byte](100))
    buffer.writeShort(3)
    buffer.writeShort(0)
    buffer.writeNullTerminatedString("user")
    buffer.writeNullTerminatedString(user)
    buffer.writeNullTerminatedString("database")
    buffer.writeNullTerminatedString(database)
    buffer.writeNull
    val bytes = buffer.toBytes

    Packet(None, Buffer(bytes))
  }
}

object StartupMessages {

  def apply(packet: Packet): Future[StartupMessages] = {
    val bytes = packet.body.underlying.array
    if(bytes sameElements AuthenticationOkBytes) {
      return Future.value(AuthenticationOk)
    } else if(bytes sameElements AuthenticationClearTxtPasswdBytes) {
      println("Authentication clear txt passwd")
      return Future.value(AuthenticationClearTxtPasswd)
    }

    return Future.exception(new Exception())
  }

  private[this] val AuthenticationOkBytes             = Array[Byte](0x0,0x0,0x0,0x0)
  private[this] val AuthenticationClearTxtPasswdBytes = Array[Byte](0x0,0x0,0x0,0x3)
}

case object AuthenticationOk extends StartupMessages
case object AuthenticationClearTxtPasswd extends StartupMessages

case class ParameterStatusMessage(parameter: String, value: String) extends Message
object ParameterStatusMessage {
  def apply(packet: Packet): Future[ParameterStatusMessage] = {
    val br = BufferReader(packet.body)
    val param = br.readNullTerminatedString()
    val value = br.readNullTerminatedString()
    val ms = new ParameterStatusMessage(param, value)
    println(ms)
    Future.value(ms)
  }
}

case class BackendKeyData(processId: Int, secretKey: Int) extends Message
object BackendKeyData {
  def apply(packet: Packet): Future[BackendKeyData] = {
    val br = BufferReader(packet.body)
    val procId = br.readInt
    val secretKey = br.readInt
    val bkd = new BackendKeyData(procId, secretKey)
    println(bkd)
    Future.value(bkd)
  }
}

case class ReadyForQuery(transactionStatus: Char) extends Message
object ReadyForQuery {
  def apply(packet: Packet): Future[ReadyForQuery] = {
    val br = BufferReader(packet.body)
    val tStatus = br.readByte
    val rfq = new ReadyForQuery(tStatus.toChar)
    Future.value(rfq)
  }
}


final class Query(s: String) extends Message {

  override def toPacket: Packet = {
    val length = s.getBytes.length
    val bw = BufferWriter(new Array[Byte](length + 1))
    bw.writeNullTerminatedString(s)
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
          val formatCode = br.readShort 

          val rdf = RowDescriptionField(name, tableObjectId, tableAttributeId, dataTypeObjectId,
            dataTypeSize, typeModifier, formatCode)
          loop((currCount + 1).toShort, rdf :: fs)
        }
        case x if x >= numFields => fs
      }

    val fs = loop(0, List.empty[RowDescriptionField]).reverse
    println(s"Number of Fields $numFields")
    fs.foreach(x => println(x))
    val rd = RowDescription(numFields, fs)
    Future.value(rd)
  }
}


case class RowDescriptionField(name: String, tableObjectId: Int, tableAttributeId: Short,
  dataTypeObjectId: Int, dataTypeSize: Short, typeModifier: Int, formatCode: Short)











