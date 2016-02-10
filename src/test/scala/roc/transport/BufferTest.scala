package com.github.finagle
package transport
package roc

import org.scalatest.FunSuite

final class BufferTest extends FunSuite {

  val bytes = Array[Byte](0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x78)

  test("read Byte") {
    val br = BufferReader(bytes)
    assert(br.readByte === 0x11)
    assert(br.readByte === 0x22)
    assert(br.readByte === 0x33)
    assert(br.readByte === 0x44)
    assert(br.readByte === 0x55)
    assert(br.readByte === 0x66)
    assert(br.readByte === 0x77)
    assert(br.readByte === 0x78)
  }

  test("read Short") {
    val br = BufferReader(bytes)
    assert(br.readShort === 0x1122)
    assert(br.readShort === 0x3344)
    assert(br.readShort === 0x5566)
    assert(br.readShort === 0x7778)
  }

  test("read Int24") {
    val br = BufferReader(bytes)
    assert(br.readInt24 === 0x112233)
    assert(br.readInt24 === 0x445566)
    assert(br.readShort === 0x7778)
  }

  test("read Int") {
    val br = BufferReader(bytes)
    assert(br.readInt === 0x11223344)
    assert(br.readInt === 0x55667778)
  }

  test("read Signed Int") {
    val n = 0xfff6ffff
    val br = BufferReader(Array[Byte](0xff.toByte, 0xf6.toByte, 0xff.toByte, 0xff.toByte))
    assert(n === br.readInt)
  }

  test("read Long") {
    val br = BufferReader(bytes)
    assert(br.readLong === 0x1122334455667778L)
  }

  test("read null terminated string") {
    val str = "Null Terminated String\u0000"
    val br = BufferReader(str.getBytes)
    assert(str.take(str.size-1) === br.readNullTerminatedString())
  }
}
