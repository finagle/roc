package roc
package postgresql
package transport

import org.specs2.Specification

final class PacketSpec extends Specification { def is = s2"""

  Packets should
    encode a 'Message Type' byte                            $encodeMessageTypeByte
    encode the length                                       $encodeLength
    encode the body                                         $encodeBody
    encode a packet with no 'Message Type' byte             $encodeNoMessageTypeByte
                                                            """

  def encodeMessageTypeByte = {
    val buf       = Buffer.fromChannelBuffer(packet.toChannelBuffer)
    val bufReader = BufferReader(buf)

    bufReader.readByte must_== 'R'
  }

  def encodeLength = {
    val buf       = Buffer.fromChannelBuffer(packet.toChannelBuffer)
    val bufReader = BufferReader(buf)
    val _ = bufReader.readByte // tested in encodeMessageTypeByte

    bufReader.readInt must_== 8
  }
  
  def encodeBody = {
    val buf       = Buffer.fromChannelBuffer(packet.toChannelBuffer)
    val bufReader = BufferReader(buf)
    bufReader.readByte // tested in encodeMessageTypeByte
    bufReader.readInt  // tested in encodeLength

    bufReader.readInt must_== 0
  }

  def encodeNoMessageTypeByte = {
    val versionBytes      = Array[Byte](0x0, 0x3, 0x0, 0x0, 0x13)
    val userBytes         = Array[Byte](0x75, 0x73, 0x65, 0x72, 0x0)
    val testUserBytes     = Array[Byte](0x74, 0x65, 0x73, 0x74, 0x55, 0x73, 0x65, 0x72, 0x0)
    val databaseBytes     = Array[Byte](0x64, 0x61, 0x74, 0x61, 0x62, 0x61, 0x73, 0x65, 0x0)
    val databaseNameBytes = Array[Byte](0x74, 0x65, 0x73, 0x74, 0x5F, 0x64, 0x61, 0x74, 0x61, 0x62,
      0x61, 0x73, 0x65, 0x0)
    val bs        = versionBytes ++ userBytes ++ testUserBytes ++ databaseBytes ++ databaseNameBytes
    val p         = Packet(None, Buffer(bs))
    val buf       = Buffer.fromChannelBuffer(p.toChannelBuffer)
    val bufReader = BufferReader(buf)

    val _ = bufReader.readInt
    bufReader.readInt must_== 196608
  }

  private val messageType = Some('R')
  private val bytes       = Array[Byte](0x0,0x0,0x0,0x0)
  private val packet      = Packet(messageType, Buffer(bytes))
}
