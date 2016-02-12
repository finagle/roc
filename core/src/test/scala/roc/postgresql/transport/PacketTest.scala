package com.github.finagle
package roc
package postgresql
package transport

import org.scalatest.FunSuite

final class PacketTest extends FunSuite {

  test("Encode a Packet") {
    val messageType = Some('R')
    val bytes = Array[Byte](0x0,0x0,0x0,0x0)
    val packet = Packet(messageType, Buffer(bytes))

    val buf = Buffer.fromChannelBuffer(packet.toChannelBuffer)
    val br = BufferReader(buf)
    assert(br.readByte === 'R')
    assert(br.readInt === 8)
    assert(br.readInt === 0)
  }

  test("Encode a Packet with no message type") {
    val messageType       = None
    val versionBytes      = Array[Byte](0x0, 0x3, 0x0, 0x0, 0x13)
    val userBytes         = Array[Byte](0x75, 0x73, 0x65, 0x72, 0x0)
    val testUserBytes     = Array[Byte](0x74, 0x65, 0x73, 0x74, 0x55, 0x73, 0x65, 0x72, 0x0)
    val databaseBytes     = Array[Byte](0x64, 0x61, 0x74, 0x61, 0x62, 0x61, 0x73, 0x65, 0x0)
    val databaseNameBytes = Array[Byte](0x74, 0x65, 0x73, 0x74, 0x5F, 0x64, 0x61, 0x74, 0x61, 0x62,
      0x61, 0x73, 0x65, 0x0)
    val bytes = versionBytes ++ userBytes ++ testUserBytes ++ databaseBytes ++ databaseNameBytes
    val packet = Packet(messageType, Buffer(bytes))
    val buf = Buffer.fromChannelBuffer(packet.toChannelBuffer)
    val br = BufferReader(buf)

    val _ = br.readInt
    assert(br.readInt === 196608)
    assert(br.readNullTerminatedString().trim === "user")
    assert(br.readNullTerminatedString() === "testUser")
    assert(br.readNullTerminatedString() === "database")
    assert(br.readNullTerminatedString() === "test_database")
  }
}
