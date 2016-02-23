package com.github.finagle
package roc
package postgresql

import cats.data.Xor

trait ByteDecoder[A] {
  def fromText(bytes: Option[Array[Byte]]): ByteDecodingFailure Xor Option[A]
  def fromBinary(bytes: Option[Array[Byte]]): Error Xor Option[A]
}

sealed trait FormatCode
case object Text extends FormatCode
case object Binary extends FormatCode

object `package` extends ByteDecoderImplicits

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

    def fromBinary(bytes: Option[Array[Byte]]): Error Xor Option[Int] =
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

    def fromBinary(bytes: Option[Array[Byte]]): Error Xor Option[String] =
      Xor.Left(new UnsupportedDecodingFailure("Decoding string from Binary is not Supported"))
  }
}
