package roc
package postgresql
package transport

import org.specs2._
import org.specs2.specification.core._
import org.specs2.specification.create.FragmentsFactory

final class BufferWriterSpec extends Specification { def is = s2"""

  BufferWriter should
    write Byte to an Array                                  ${WriterCtx(1).writeByte}
    write Short to an Array                                 ${WriterCtx(2).writeShort}
    write Int24 to an Array                                 ${WriterCtx(3).writeInt24}
    write Int to an Array                                   ${WriterCtx(4).writeInt}
    write Long to an Array                                  ${WriterCtx(8).writeLong}
    write Tiny Length coded Binary to an Array              ${WriterCtx(9).writeTinyLengthCodedBinary}
    write Short Length coded Binary to an Array             ${WriterCtx(9).writeShortLengthCodedBinary}
    write Medium Length coded Binary to an Array            ${WriterCtx(9).writeMediumLengthCodedBinary}
    write Large Length coded Binary to an Array             ${WriterCtx(9).writeLargeLengthCodedBinary}
    write C-Style String to an Array                        ${WriterCtx(9).writeCStyleString}
    write Tiny Length coded String to an Array              ${WriterCtx(9).writeTinyLengthCodedString}
    write Short Length coded String to an Array             ${WriterCtx(9).writeShortLengthCodedString}
    write Coded String with non-ascii characters            ${WriterCtx(9).writeCodedStringWithNonAsciiChars}
                                                            """
  
  case class WriterCtx(len: Int) {
    def writeByte = {
      bw.writeByte(0x01.toByte)
      br.readByte must_== 0x01
    }

    def writeShort = {
      bw.writeShort(0xFE.toShort)
      br.readShort must_== 0xFE
    }

    def writeInt24 = {
      bw.writeInt24(0x872312)
      br.readUnsignedInt24 must_== 0x872312
    }

    def writeInt = {
      bw.writeInt(0x98765432)
      br.readInt must_== 0x98765432
    }

    def writeLong = {
      bw.writeLong(0x7877665544332211L)
      br.readLong must_== 0x7877665544332211L
    }

    def writeTinyLengthCodedBinary = {
      bw.writeLengthCodedBinary(250)
      br.readLengthCodedBinary must_== 250
    }

    def writeShortLengthCodedBinary = {
      bw.writeLengthCodedBinary(65535)
      br.readLengthCodedBinary must_== 65535
    }

    def writeMediumLengthCodedBinary = {
      bw.writeLengthCodedBinary(16777215)
      br.readLengthCodedBinary must_== 16777215
    }

    def writeLargeLengthCodedBinary = {
      bw.writeLengthCodedBinary(16777217L)
      br.readLengthCodedBinary must_== 16777217L
    }

    def writeCStyleString = {
      val str = "test\u0000"
      bw.writeNullTerminatedString(str)
      br.readNullTerminatedString() must_== str.take(str.length - 1)
    }

    def writeTinyLengthCodedString = {
      val str = "test"
      bw.writeLengthCodedString(str)
      br.readLengthCodedString() must_== str
    }

    def writeShortLengthCodedString = {
      val str = "test" * 100
      val len = Buffer.sizeOfLen(str.size) + str.size
      val strAsBytes = new Array[Byte](len)
      val bw = BufferWriter(strAsBytes)
      bw.writeLengthCodedString(str)

      val br = BufferReader(strAsBytes)
      br.readLengthCodedString() must_== str
    }

    def writeCodedStringWithNonAsciiChars = {
      val str = "バイトルドットコム"
      val strAsBytes = new Array[Byte](100)
      val bw = BufferWriter(strAsBytes)
      bw.writeLengthCodedString(str)
      val br = BufferReader(strAsBytes)

      br.readLengthCodedString() must_== str
    }

    private val bytes = new Array[Byte](len)
    private val bw    = BufferWriter(bytes)
    private val br    = BufferReader(bytes)
    private val ff = fragmentFactory
  }
}
