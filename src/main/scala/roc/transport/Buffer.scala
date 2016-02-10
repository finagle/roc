package com.github.finagle.transport
package roc

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import java.nio.ByteOrder
import java.nio.charset.Charset


object Buffer {

  def apply(bytes: Array[Byte]): Buffer = new Buffer {
    val underlying = ChannelBuffers.wrappedBuffer(bytes)
  }

  def fromChannelBuffer(cb: ChannelBuffer): Buffer = {
    require(cb != null)
    require(cb.order == ByteOrder.BIG_ENDIAN, "Invalid ChannelBuffer ByteOrder")
    new Buffer { val underlying = cb }
  }

}

sealed trait Buffer {
  val underlying: ChannelBuffer
  def capacity: Int = underlying.capacity
}

trait BufferReader extends Buffer {

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
   * Returns the bytes from start to start+length
   * into a string using the given java.nio.charset.Charset.
   */
  def toString(start: Int, length: Int, charset: Charset): String
}

object BufferReader {

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
