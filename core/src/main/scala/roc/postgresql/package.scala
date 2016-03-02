package roc

import roc.postgresql.transport.{PacketDecoder, PacketDecoderImplicits, Packet, PacketEncoder, 
  PacketEncoderImplicits}
import java.nio.charset.StandardCharsets

package object postgresql
  extends ByteDecoderImplicits
  with PacketEncoderImplicits 
  with PacketDecoderImplicits {

  def encodePacket[A <: FrontendMessage: PacketEncoder](a: A): Packet = 
    implicitly[PacketEncoder[A]].apply(a)

  def decodePacket[A <: BackendMessage: PacketDecoder](p: Packet): PacketDecoder.Result[A] =
    implicitly[PacketDecoder[A]].apply(p)

  def lengthOfCStyleString(str: String): Int = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    bytes.length + 1
  }

  def lengthOfCStyleStrings(xs: List[String]): Int = xs match {
    case h :: t => xs.map(lengthOfCStyleString).reduce(_ + _)
    case t      => 0
  }

}
