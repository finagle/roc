package roc
package postgresql

import cats.data.Xor
import roc.postgresql.failures.{ByteDecodingFailure, Failure, UnsupportedDecodingFailure}

trait ByteDecoder[A] {
  def fromText(bytes: Option[Array[Byte]]): Xor[ByteDecodingFailure, Option[A]]
  def fromBinary(bytes: Option[Array[Byte]]): Xor[Failure, Option[A]]
}

trait ByteDecoderImplicits {

  implicit val intByteDecoder: ByteDecoder[Int] = new ByteDecoder[Int] {
    def fromText(bytes: Option[Array[Byte]]): ByteDecodingFailure Xor Option[Int] =
      bytes match {
        case Some(b) => {
          val int = b.map(_.toChar).mkString.toInt
          Xor.Right(Some(int))
        }
        case None    => Xor.Right(None) 
      }

    def fromBinary(bytes: Option[Array[Byte]]): Xor[Failure, Option[Int]] =
      Xor.Left(new UnsupportedDecodingFailure("Decoding int from Binary is not Supported"))
  }

  implicit val stringByteDecoder: ByteDecoder[String] = new ByteDecoder[String] {
    def fromText(bytes: Option[Array[Byte]]): ByteDecodingFailure Xor Option[String] = 
      bytes match {
        case Some(b) => {
          val str = b.map(_.toChar).mkString
          Xor.Right(Some(str))
        }
        case None    => Xor.Right(None)
      }

    def fromBinary(bytes: Option[Array[Byte]]): Xor[Failure, Option[String]] =
      Xor.Left(new UnsupportedDecodingFailure("Decoding string from Binary is not Supported"))
  }
}
