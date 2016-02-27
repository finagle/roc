package com.github.finagle
package roc
package postgresql

import com.github.finagle.roc.postgresql.transport.{Buffer, BufferWriter, Packet}
import java.nio.charset.StandardCharsets

private[postgresql] trait PacketEncoder[A <: FrontendMessage] {
  def apply(a: A): Packet
}

private[postgresql] object PacketEncoder {
  def lengthOfStartupMessageByteArray(sm: StartupMessage): Int = {
    val protocolLength  = 4 //2 shorts * 2
    val lengthOfUserLbl = lengthOfCStyleString("user")
    val lengthOfUser    = lengthOfCStyleString(sm.user)
    val lengthOfDbLbl   = lengthOfCStyleString("database")
    val lengthOfDb      = lengthOfCStyleString(sm.database)
    val extraNull       = 1

    protocolLength + lengthOfUserLbl + lengthOfUser + lengthOfDbLbl + lengthOfDb + extraNull
  }

}

private[postgresql] trait PacketEncoderImplicits {
  import PacketEncoder._

  implicit val startupMessageEncoder: PacketEncoder[StartupMessage] = 
    new PacketEncoder[StartupMessage] {
      def apply(sm: StartupMessage): Packet = {
        val buffer = BufferWriter(new Array[Byte](lengthOfStartupMessageByteArray(sm)))
        buffer.writeShort(3)
        buffer.writeShort(0)
        buffer.writeNullTerminatedString("user")
        buffer.writeNullTerminatedString(sm.user)
        buffer.writeNullTerminatedString("database")
        buffer.writeNullTerminatedString(sm.database)
        buffer.writeNull
        Packet(None, Buffer(buffer.toBytes))
      }
    }

  implicit val passwordMessageEncoder: PacketEncoder[PasswordMessage] = 
    new PacketEncoder[PasswordMessage] {
      def apply(pm: PasswordMessage): Packet = {
        val length = pm.password.getBytes(StandardCharsets.UTF_8).length
        val bw = BufferWriter(new Array[Byte](length + 1))
        bw.writeNullTerminatedString(pm.password)
        Packet(Some(Message.PasswordMessageByte), Buffer(bw.toBytes))
      }
    }

  implicit val queryMessageEncoder: PacketEncoder[Query] = 
    new PacketEncoder[Query] {
      def apply(q: Query): Packet = {
        val length = q.queryString.getBytes(StandardCharsets.UTF_8).length
        val bw     = BufferWriter(new Array[Byte](length + 1))
        bw.writeNullTerminatedString(q.queryString)
        val bytes  = bw.toBytes
        Packet(Some(Message.QueryMessageByte), Buffer(bytes))
      }
    }
}
