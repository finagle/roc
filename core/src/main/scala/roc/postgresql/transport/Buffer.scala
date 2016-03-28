package roc
package postgresql
package transport

import java.nio.ByteOrder
import java.nio.charset.Charset
import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}

private[postgresql] object Buffer {
  val NullLength = -1 // denotes a SQL NULL value when reading a length coded binary.
  val EmptyString = new String

  /**
   * Calculates the size required to store a length
   * according to the MySQL protocol for length coded
   * binary.
   */
  def sizeOfLen(l: Long) =
    if (l < 251) 1 else if (l < 65536) 3 else if (l < 16777216) 4 else 9

  def apply(bytes: Array[Byte]): Buffer = new Buffer {
    val underlying = ChannelBuffers.wrappedBuffer(bytes)
  }

  def fromChannelBuffer(cb: ChannelBuffer): Buffer = {
    require(cb != null)
    require(cb.order == ByteOrder.BIG_ENDIAN, "Invalid ChannelBuffer ByteOrder")
    new Buffer { val underlying = cb }
  }

}

private[postgresql] sealed trait Buffer {
  val underlying: ChannelBuffer
  def capacity: Int = underlying.capacity
}

private[postgresql] trait BufferReader extends Buffer {

  /** Current reader offset in the buffer. */
  def offset: Int

  /**
   * Denotes if the buffer is readable upto the given width
   * based on the current offset.
   */
  def readable(width: Int): Boolean

  def readByte: Byte
  def readUnsignedByte: Short
  def readShort: Short
  def readUnsignedShort: Int
  def readInt24: Int
  def readUnsignedInt24: Int
  def readInt: Int
  def readUnsignedInt: Long
  def readLong: Long
  def readFloat: Float
  def readDouble: Double

  /**
   * Increases offset by n.
   */
  def skip(n: Int): Unit

  /**
   * Consumes the rest of the buffer and returns
   * it in a new Array[Byte].
   * @return Array[Byte] containing the rest of the buffer.
   */
  def takeRest(): Array[Byte] = take(capacity - offset)

  /**
   * Consumes n bytes in the buffer and
   * returns them in a new Array.
   * @return An Array[Byte] containing bytes from offset to offset+n
   */
  def take(n: Int): Array[Byte]

  /**
   * Reads a MySQL data field. A variable-length numeric value.
   * Depending on the first byte, reads a different width from
   * the buffer. For more info, refer to MySQL Client/Server protocol
   * documentation.
   * @return a numeric value representing the number of
   * bytes expected to follow.
   */
  def readLengthCodedBinary: Long = readUnsignedByte match {
    case byte if byte < 251  => byte
    case byte if byte == 251 => Buffer.NullLength
    case byte if byte == 252 => readUnsignedShort
    case byte if byte == 253 => readUnsignedInt24
    case byte if byte == 254 => readLong
    case _                   => throw new IllegalStateException("Invalid length byte")
  }

  /**
   * Reads a null-terminated string where
   * null is denoted by '\0'. Uses Charset.defaultCharset by default
   * to decode strings.
   * @return a null-terminated String starting at offset.
   */
  def readNullTerminatedString(charset: Charset = Charset.defaultCharset): String = {
    val start = offset
    var length = 0

    while (readByte != 0x00)
      length += 1

    this.toString(start, length, charset)
  }

  /**
   * Reads a length encoded string according to the MySQL
   * Client/Server protocol. Uses Charset.defaultCharset by default
   * to decode strings. For more details refer to MySQL
   * documentation.
   * @return a MySQL length coded String starting at
   * offset.
   */
  def readLengthCodedString(charset: Charset = Charset.defaultCharset): String = {
    val length = readLengthCodedBinary.toInt
    if (length == Buffer.NullLength)
       null
    else if (length == 0)
      Buffer.EmptyString
    else {
      val start = offset
      skip(length)
      this.toString(start, length, charset)
    }
  }



  /**
   * Returns the bytes from start to start+length
   * into a string using the given java.nio.charset.Charset.
   */
  def toString(start: Int, length: Int, charset: Charset): String
}

private[postgresql] object BufferReader {

  def apply(buf: Buffer, offset: Int = 0): BufferReader = {
    require(offset >= 0, "Invalid reader offset")
    buf.underlying.readerIndex(offset)
    new Netty3BufferReader(buf.underlying)
  }

  def apply(bytes: Array[Byte]): BufferReader =
    apply(Buffer(bytes), 0)

  /**
   * BufferReader implementation backed by a Netty3 ChannelBuffer.
   */
  private[this] final class Netty3BufferReader(val underlying: ChannelBuffer)
    extends BufferReader with Buffer {
    def offset: Int          = underlying.readerIndex
    def readable(width: Int) = underlying.readableBytes >= width

    def readByte: Byte          = underlying.readByte()
    def readUnsignedByte: Short = underlying.readUnsignedByte()
    def readShort: Short        = underlying.readShort()
    def readUnsignedShort: Int  = underlying.readUnsignedShort()
    def readInt24: Int          = underlying.readMedium()
    def readUnsignedInt24: Int  = underlying.readUnsignedMedium()
    def readInt: Int            = underlying.readInt()
    def readUnsignedInt: Long   = underlying.readUnsignedInt()
    def readLong: Long          = underlying.readLong()
    def readFloat: Float        = underlying.readFloat()
    def readDouble: Double      = underlying.readDouble()

    def skip(n: Int) = underlying.skipBytes(n)

    def take(n: Int) = {
      val res = new Array[Byte](n)
      underlying.readBytes(res)
      res
    }

    def toString(start: Int, length: Int, charset: Charset) =
      underlying.toString(start, length, charset)
  }
}

/**
 * Provides convenient methods for writing the
 * data in a postgresql packet body. All data is encoded
 * in big endian byte order in accordance with
 * the mysql protocol. Operations are side-effecting,
 * that is, all operations increase the offset
 * into the underlying buffer.
 */
private[postgresql] trait BufferWriter extends Buffer {

  /**
   * Current writer offset.
   */
  def offset: Int

  /**
   * Denotes if the buffer is writable upto the given width
   * based on the current offset.
   */
  def writable(width: Int): Boolean

  def writeBoolean(b: Boolean): BufferWriter
  def writeByte(n: Int): BufferWriter
  def writeShort(n: Int): BufferWriter
  def writeInt24(n: Int): BufferWriter
  def writeInt(n: Int): BufferWriter
  def writeLong(n: Long): BufferWriter
  def writeFloat(f: Float): BufferWriter
  def writeDouble(d: Double): BufferWriter
  def writeNull: BufferWriter

  def skip(n: Int): BufferWriter

  def toBytes: Array[Byte]
  /**
   * Fills the rest of the buffer with the given byte.
   * @param b Byte used to fill.
   */
  def fillRest(b: Byte) = fill(capacity - offset, b)

  /**
   * Fills the buffer from current offset to offset+n with b.
   * @param n width to fill
   * @param b Byte used to fill.
   */
  def fill(n: Int, b: Byte) = {
    (offset until offset + n) foreach { j => writeByte(b) }
    this
  }

  /**
   * Writes bytes onto the buffer.
   * @param bytes Array[Byte] to copy onto the buffer.
   */
   def writeBytes(bytes: Array[Byte]): BufferWriter

   /**
    * Writes a length coded binary according the the MySQL
    * Client/Server protocol. Refer to MySQL documentation for
    * more information.
    */
   def writeLengthCodedBinary(length: Long): BufferWriter = {
     if (length < 251) {
        writeByte(length.toInt)
      } else if (length < 65536) {
        writeByte(252)
        writeShort(length.toInt)
      } else if (length < 16777216) {
        writeByte(253)
        writeInt24(length.toInt)
      } else {
        writeByte(254)
        writeLong(length)
      }
    }

   /**
    * Writes a null terminated string onto the buffer where
    * '\0' denotes null. Uses Charset.defaultCharset by default
    * to decode the given String.
    * @param s String to write.
    */
   def writeNullTerminatedString(
     s: String,
     charset: Charset = Charset.defaultCharset
   ): BufferWriter = {
    writeBytes(s.getBytes(charset))
    writeByte('\u0000')
    this
   }

   /**
    * Writes a length coded string using the MySQL Client/Server
    * protocol. Uses Charset.defaultCharset by default to decode
    * the given String.
    * @param s String to write to buffer.
    */
   def writeLengthCodedString(s: String,
     charset: Charset = Charset.defaultCharset
   ): BufferWriter = writeLengthCodedBytes(s.getBytes(charset))

   /**
    * Writes a length coded set of bytes according to the MySQL
    * client/server protocol.
    */
   def writeLengthCodedBytes(bytes: Array[Byte]): BufferWriter = {
    writeLengthCodedBinary(bytes.length)
    writeBytes(bytes)
    this
   }
}

private[postgresql] object BufferWriter {

  def apply(buf: Buffer, offset: Int = 0): BufferWriter = {
    require(offset >= 0, "Inavlid writer offset.")
    buf.underlying.writerIndex(offset)
    new Netty3BufferWriter(buf.underlying)
  }

  def apply(bytes: Array[Byte]): BufferWriter =
    apply(Buffer(bytes), 0)

  /**
   * BufferWriter implementation backed by a Netty ChannelBuffer.
   */
  private[this] class Netty3BufferWriter(val underlying: ChannelBuffer)
    extends BufferWriter with Buffer {
    def offset = underlying.writerIndex
    def writable(width: Int = 1): Boolean = underlying.writableBytes >= width

    def writeBoolean(b: Boolean): BufferWriter = if(b) writeByte(1) else writeByte(0)

    def writeByte(n: Int): BufferWriter = {
      underlying.writeByte(n)
      this
    }

    def writeShort(n: Int): BufferWriter = {
      underlying.writeShort(n)
      this
    }

    def writeInt24(n: Int): BufferWriter = {
      underlying.writeMedium(n)
      this
    }

    def writeInt(n: Int): BufferWriter = {
      underlying.writeInt(n)
      this
    }

    def writeLong(n: Long): BufferWriter = {
      underlying.writeLong(n)
      this
    }

    def writeFloat(f: Float): BufferWriter = {
      underlying.writeFloat(f)
      this
    }

    def writeDouble(d: Double): BufferWriter = {
      underlying.writeDouble(d)
      this
    }

    def skip(n: Int) = {
      underlying.writerIndex(offset + n)
      this
    }

    def writeBytes(bytes: Array[Byte]) = {
      underlying.writeBytes(bytes)
      this
    }

    def writeNull: BufferWriter = {
      underlying.writeZero(1)
      this
    }

    def toBytes: Array[Byte] = {
      val bytes = new Array[Byte](underlying.writerIndex)
      underlying.getBytes(0, bytes)
      bytes
    }
  }
}
