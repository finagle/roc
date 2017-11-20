package roc
package postgresql
package transport

import org.specs2.Specification
import org.specs2.specification.core._

final class BufferReaderSpec extends Specification { def is = s2"""

  BufferReader should
    read Bytes from an Array                                $readBytes
    read Shorts from an Array                               $readShorts
    read Int24s and remaining Short from an Array           $readInt24s
    read Ints from an Array                                 $readInts
    read Signed Int from an Array                           $readSignedInt
    read Long from an Array                                 $readLong
    read C-Style String from an Array                       $readNullTerminatedString
                                                            """

  def readBytes = {
    val br    = BufferReader(bytes)
    val exec1 = br.readByte must_== 0x11
    val exec2 = br.readByte must_== 0x22
    val exec3 = br.readByte must_== 0x33
    val exec4 = br.readByte must_== 0x44
    val exec5 = br.readByte must_== 0x55
    val exec6 = br.readByte must_== 0x66
    val exec7 = br.readByte must_== 0x77
    val exec8 = br.readByte must_== 0x78
    
    Fragments(ff.break, ff.example("read 1st byte".formatted("%19s"), exec1))
      .append(Fragments(ff.break, ff.example("read 2nd byte".formatted("%19s"), exec2)))
      .append(Fragments(ff.break, ff.example("read 3rd byte".formatted("%19s"), exec3)))
      .append(Fragments(ff.break, ff.example("read 4th byte".formatted("%19s"), exec4)))
      .append(Fragments(ff.break, ff.example("read 5th byte".formatted("%19s"), exec5)))
      .append(Fragments(ff.break, ff.example("read 6th byte".formatted("%19s"), exec6)))
      .append(Fragments(ff.break, ff.example("read 7th byte".formatted("%19s"), exec7)))
      .append(Fragments(ff.break, ff.example("read 8th byte".formatted("%19s"), exec8)))
  }

  def readShorts = {
    val br    = BufferReader(bytes)
    val exec1 = br.readShort must_== 0x1122
    val exec2 = br.readShort must_== 0x3344
    val exec3 = br.readShort must_== 0x5566
    val exec4 = br.readShort must_== 0x7778
    
    Fragments(ff.break, ff.example("read 1st short".formatted("%20s"), exec1))
      .append(Fragments(ff.break, ff.example("read 2nd short".formatted("%20s"), exec2)))
      .append(Fragments(ff.break, ff.example("read 3rd short".formatted("%20s"), exec3)))
      .append(Fragments(ff.break, ff.example("read 4th short".formatted("%20s"), exec4)))
  }

  def readInt24s = {
    val br    = BufferReader(bytes)
    val exec1 = br.readInt24 must_== 0x112233
    val exec2 = br.readInt24 must_== 0x445566
    val exec3 = br.readShort must_== 0x7778
   
    Fragments(ff.break, ff.example("read 1st Int24".formatted("%20s"), exec1))
      .append(Fragments(ff.break, ff.example("read 2nd Int24".formatted("%20s"), exec2)))
      .append(Fragments(ff.break, ff.example("read remaining Short".formatted("%26s"), exec3)))
  }

  def readInts = {
    val br    = BufferReader(bytes)
    val exec1 = br.readInt must_== 0x11223344
    val exec2 = br.readInt must_== 0x55667778

    Fragments(ff.break, ff.example("read 1st Int".formatted("%18s"), exec1))
      .append(Fragments(ff.break, ff.example("read 2nd Int".formatted("%18s"), exec2)))
  }

  def readSignedInt = {
    val n = 0xfff6ffff
    val br = BufferReader(Array[Byte](0xff.toByte, 0xf6.toByte, 0xff.toByte, 0xff.toByte))
    br.readInt must_== n 
  }

  def readLong = {
    val br = BufferReader(bytes)
    br.readLong must_== 0x1122334455667778L
  }

  def readNullTerminatedString = {
    val str = "Null Terminated String\u0000"
    val br = BufferReader(str.getBytes)
    br.readNullTerminatedString() must_== str.take(str.size-1)
  }

  private val bytes = Array[Byte](0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x78)
  override val ff = fragmentFactory
}
