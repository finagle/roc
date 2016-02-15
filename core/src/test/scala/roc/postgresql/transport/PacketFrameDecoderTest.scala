package com.github.finagle
package roc
package postgresql
package transport

import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.channel.{Channel, ChannelHandlerContext}
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar

final class PacketFrameDecoderTest extends FunSuite with MockitoSugar {
  val ctx = mock[ChannelHandlerContext]
  val c = mock[Channel]
  val frameDecoder = new PacketFrameDecoder

  test("ignore incomplete packets") {
    val partial = Array[Byte](0x05, 0x00)
    val result = frameDecoder.decode(ctx, c, ChannelBuffers.wrappedBuffer(partial))
    assert(result === null)
  }

  test("decode complete packets") {

    val messageByte = Array[Byte](0x52)
    val lengthBytes = Array[Byte](0x0, 0x0, 0x0, 0x8)
    val okBytes     = Array[Byte](0x0, 0x0, 0x0, 0x0)
    val bytes       = messageByte ++ lengthBytes ++ okBytes

    val result = frameDecoder.decode(ctx, c, ChannelBuffers.wrappedBuffer(bytes))
    assert(result != null)
    assert(result.messageType === Some('R'))
    assert(result.body.underlying.array === okBytes)
  }
}
