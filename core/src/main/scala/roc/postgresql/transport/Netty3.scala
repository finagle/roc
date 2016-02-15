package com.github.finagle
package roc
package postgresql
package transport

import com.twitter.finagle.client.Transporter
import com.twitter.finagle.netty3.Netty3Transporter
import com.twitter.util.NonFatal
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.frame.FrameDecoder

private[roc] final class PacketFrameDecoder extends FrameDecoder {

  override def decode(ctx: ChannelHandlerContext, channel: Channel, 
                      buffer: ChannelBuffer): Packet = {
    if(buffer.readableBytes < Packet.HeaderSize) return null

    buffer.markReaderIndex()
    val code = buffer.readByte.toChar
    val length = buffer.readInt 

    if(buffer.readableBytes < length - 4) {
      buffer.resetReaderIndex()
      return null
    }

    val body = new Array[Byte](length - 4)
    buffer.readBytes(body)
    Packet(Some(code), BufferReader(body))
  }

}

private[roc] final class PacketEncoder extends SimpleChannelDownstreamHandler {
  override def writeRequested(ctx: ChannelHandlerContext, evt: MessageEvent) =
    evt.getMessage match {
      case p: Packet =>
        try {
          val cb = p.toChannelBuffer
          Channels.write(ctx, evt.getFuture, cb, evt.getRemoteAddress)
        } catch {
          case NonFatal(e) =>
            val _ = evt.getFuture.setFailure(new ChannelException(e.getMessage))
        }

      case unknown =>
        val _ = evt.getFuture.setFailure(new ChannelException(
          "Unsupported request type %s".format(unknown.getClass.getName)))
    }
}

/**
  * A Netty3 pipeline that is responsible for framing network traffic in 
  * terms of of logical postgresql packets ( see Packet )
 */
private[roc] object PostgresqlClientPipelineFactory extends ChannelPipelineFactory {
  def getPipeline = {
    val pipeline = Channels.pipeline()
    pipeline.addLast("pgPacketDecoder", new PacketFrameDecoder)
    pipeline.addLast("pgPacketEncoder", new PacketEncoder)
    pipeline
  }
}
