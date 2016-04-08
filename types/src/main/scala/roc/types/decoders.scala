package roc
package types

import cats.data.Xor
import roc.postgresql.ElementDecoder
import roc.types.failures._
import roc.postgresql.transport.BufferReader

object decoders {

  implicit def optionElementDecoder[A](implicit f: ElementDecoder[A]) = 
    new ElementDecoder[Option[A]] {
      def textDecoder(text: String): Option[A]         = Some(f.textDecoder(text))
      def binaryDecoder(bytes: Array[Byte]): Option[A] = Some(f.binaryDecoder(bytes))
      def nullDecoder(): Option[A]                     = None
    }

  implicit val stringElementDecoder: ElementDecoder[String] = new ElementDecoder[String] {
    def textDecoder(text: String): String         = text
    def binaryDecoder(bytes: Array[Byte]): String = bytes.map(_.toChar).mkString
    def nullDecoder(): String                     = throw new NullDecodedFailure("STRING")
  }

  implicit val intElementDecoder: ElementDecoder[Int] = new ElementDecoder[Int] {
    def textDecoder(text: String): Int         = Xor.catchNonFatal(
      text.toInt
    ).fold(
      {l => throw new ElementDecodingFailure("INT", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Int = Xor.catchNonFatal(
      BufferReader(bytes.take(4)).readInt
    ).fold(
      {l => throw new ElementDecodingFailure("INT", l)},
      {r => r}
    )
    def nullDecoder(): Int =                     throw new NullDecodedFailure("INT")
  }
}
