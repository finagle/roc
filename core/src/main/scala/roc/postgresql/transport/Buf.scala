package roc
package postgresql
package transport

import io.netty.buffer.{ByteBuf, Unpooled}
import java.nio.charset.{Charset, StandardCharsets}

/** A Buf based on a Netty-4 ByteBuffer
  *
  * @note As of this moment we are using Unpooled Buffers, but this may change in the near
  *     future.
  */
object Buf {

  /** Creates a buf w/ the specified length
    * @param the capacity of the Buffer
    * @return a [[Buf]] with an initial capacity set to length
    */
  def apply(length: Int): Buf     = new Buf(Unpooled.buffer(length))

  /** Creates a buf w/ the length of the bytes, and then writes the bytes to the Buffer
    * @param an [[Array[Byte]] to write to the Buffer
    * @return a [[Buf]] with the bytes already written
    */
  def apply(xs: Array[Byte]): Buf = {
    val buffer = Unpooled.buffer(xs.length)
    buffer.writeBytes(xs)
    new Buf(buffer)
  }

  /** Computes the length of an Array needed to hold all the bytes from a String + NULL
    * @param s the String to calculate the length of
    * @param charset the Charset to base this calculation off of ( defaults to UTF-8 )
    * @return length an `Int` representing the length of an array
    * @note a NULL terminated string is referred to as a C-Style string
    */
  def lengthOfCStyleString(s: String, charset: Charset = StandardCharsets.UTF_8): Int =
    s.getBytes(charset).length + 1

  /** Helper method for lenghtOfCStyleString
    */
  def lengthOfCStyleStrings(xs: List[String], charset: Charset = StandardCharsets.UTF_8): Int =
    xs match {
      case h :: t => xs.map(x => lengthOfCStyleString(x, charset)).reduce(_ + _)
      case t      => 0
    }
}

/** A Buffer for reading and writing primatives to an Array
  * @param underlying a to utilize. 
  * @note unlike the existing Buffer, this is both read and write compatible
  */
final class Buf private(private[this] val underlying: ByteBuf) {

  /** Read one byte from the array
    * @return Byte
    */
  def readByte: Byte = underlying.readByte

  /** Reads the number of bytes of the given as an argument from the Buffer
    * @param count the number of bytes to read
    * @return bytes an `Array[Byte]` with the requested number of Bytes
    */
  def readBytes(length: Int): Array[Byte] = {
    val xs = Array.fill[Byte](length)(0x00)
    underlying.readBytes(xs)
    xs
  }

  /** Reads a C-Style String from the Buffer
    * @param charset the Charset to use in decoding Bytes. Defaults to UTF-8
    * @return the converted String
    */
  def readCStyleString(charset: Charset = StandardCharsets.UTF_8): String = {
    val idx = underlying.bytesBefore(0x00)
    val xs  = Array.fill[Byte](idx - underlying.readerIndex)(0x00)
    underlying.readBytes(xs)
    new String(xs, charset)
  }

  /** Reads a Double from the Buffer
    * @return the double read
    */
  def readDouble: Double = underlying.readDouble

  /** Reads a Float from the Buffer
    * @return the float read
    */
  def readFloat: Float = underlying.readFloat

  /** Reads an Int from the Buffer
    * @return the int read
    */
  def readInt: Int = underlying.readInt
  
  /** Reads a Long from the Buffer
    * @return the long read
    */
  def readLong: Long = underlying.readLong

  /** Reads a Short from the Buffer
    * @return the short read
    */
  def readShort: Short = underlying.readShort

  /** Convertes the readable portion of this Buffer to an `Array[Byte]`
    * @return xs the viewable portion of this buffer as a byte array
    */
  def toBytes: Array[Byte] = {
    val length = underlying.writerIndex - underlying.readerIndex
    val xs = Array.fill[Byte](length)(0x00)
    underlying.readBytes(xs)
    xs
  }

  /** Writes a byte to the buffer
    * @param b to the Byte to write
    */
  def writeByte(b: Byte): Unit = {
    underlying.writeByte(b)
    ()
  }

  /** Writes an Array of bytes to the buffer
    * @param xs an `Array[Byte]`
    */
  def writeBytes(xs: Array[Byte]): Unit = {
    underlying.writeBytes(xs)
    ()
  }

  /** Writes a String to the Buffer using the given Charset, then writes a terminated NULL
    * @param s the String to write to the Buffer
    * @param charset the Charset used in coverting a Char => Byte(s)
    */
  def writeCStyleString(s: String, charset: Charset = StandardCharsets.UTF_8): Unit = {
    val bytes = s.getBytes(charset)
    underlying.writeBytes(bytes)
    underlying.writeZero(1)
    ()
  }

  /** Writes an 8-Byte Double to Buffer
    * @param d the double to write
    */
  def writeDouble(d: Double): Unit = {
    underlying.writeDouble(d)
    ()
  }

  /** Writes a 4-Byte Float to Buffer
    * @param f the float to write
    */
  def writeFloat(f: Float): Unit = {
    underlying.writeFloat(f)
    ()
  }

  /** Writes a 4-Byte Int to Buffer
    * @param i the long to write
    */
  def writeInt(i: Int): Unit = {
    underlying.writeInt(i)
    ()
  }

  /** Writes an 8-Byte long to Buffer
    * @param l the long to write
    */
  def writeLong(l: Long): Unit = {
    underlying.writeLong(l)
    ()
  }

  /** Writes a 1-Byte NULL (0x00) to the buffer
    */
  def writeNull(): Unit = {
    underlying.writeZero(1)
    ()
  }

  /** Writes a 2-Byte short to Buffer
    * @param s the short to write
    */
  def writeShort(s: Short): Unit = {
    underlying.writeShort(s)
    ()
  }
}
