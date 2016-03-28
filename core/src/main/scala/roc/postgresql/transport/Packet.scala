package roc
package postgresql
package transport

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}

private[roc] case class Packet(messageType: Option[Char], body: Buffer) {

  def length: Int = body.underlying.capacity + 4

  def toChannelBuffer: ChannelBuffer = {
    val messageByte = messageType match {
      case Some(char) => Array[Byte](char.toByte)
      case None       => Array[Byte]()
    }
    val bufferLength = messageByte.length + length
    val buffer = BufferWriter(new Array[Byte](bufferLength))
    if(messageByte.length > 0) {
      buffer.writeBytes(messageByte)
    }
    buffer.writeInt(length)
    buffer.writeBytes(body.underlying.array)
    ChannelBuffers.wrappedBuffer(buffer.toBytes)
  }

}


private[roc] object Packet {
  val HeaderSize = 5
}
