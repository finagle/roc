package roc
package postgresql
package transport

import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.channel.{Channel, ChannelHandlerContext}
import org.specs2._
import org.specs2.mock._

final class PacketFrameDecoderSpec extends Specification { def is = s2"""

  The 'PacketFrameDecoder' should
    ignore incomplete packets                               ${funcs().ignore}
    not be null when decoding complete packets              ${funcs().notNullDecode}
    decode the 'Message Type' byte                          ${funcs().messageTypeDecode}
    decode the body                                         ${funcs().bodyDecode}
                                                            """
  case class funcs() extends Mockito {
    def ignore = {
      val partial = Array[Byte](0x05, 0x00)
      val result  = frameDecoder.decode(ctx, c, ChannelBuffers.wrappedBuffer(partial))
      result must_== null
    }
    def notNullDecode = {
      val result = frameDecoder.decode(ctx, c, ChannelBuffers.wrappedBuffer(bytes))
      result must_!= null
    }
    def messageTypeDecode = {
      val result = frameDecoder.decode(ctx, c, ChannelBuffers.wrappedBuffer(bytes))
      result.messageType must_== Some('R')
    }
    def bodyDecode = {
      val result = frameDecoder.decode(ctx, c, ChannelBuffers.wrappedBuffer(bytes))
      result.body.underlying.array must_== okBytes
    }

    private val ctx          = mock[ChannelHandlerContext]
    private val c            = mock[Channel]
    private val frameDecoder = new PacketFrameDecoder
    private val messageByte  = Array[Byte](0x52)
    private val lengthBytes  = Array[Byte](0x0, 0x0, 0x0, 0x8)
    private val okBytes      = Array[Byte](0x0, 0x0, 0x0, 0x0)
    private val bytes        = messageByte ++ lengthBytes ++ okBytes
  }
}
