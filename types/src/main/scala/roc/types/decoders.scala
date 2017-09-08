package roc
package types

import cats.data.Validated
import cats.implicits._
import io.netty.buffer.Unpooled
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.{LocalDate, LocalTime, ZonedDateTime}
import jawn.ast.JParser
import roc.postgresql.{ElementDecoder, Null}
import roc.types.failures._

object decoders {

  implicit def optionElementDecoder[A](implicit f: ElementDecoder[A]) =
    new ElementDecoder[Option[A]] {
      def textDecoder(text: String): Option[A]         = Some(f.textDecoder(text))
      def binaryDecoder(bytes: Array[Byte]): Option[A] = Some(f.binaryDecoder(bytes))
      def nullDecoder(n: Null): Option[A]                     = None
    }

  implicit val stringElementDecoder: ElementDecoder[String] = new ElementDecoder[String] {
    def textDecoder(text: String): String         = text
    def binaryDecoder(bytes: Array[Byte]): String = bytes.map(_.toChar).mkString
    def nullDecoder(n: Null): String              = throw new NullDecodedFailure("STRING")
  }

  implicit val shortElementDecoder: ElementDecoder[Short] = new ElementDecoder[Short] {
    def textDecoder(text: String): Short         = Either.catchNonFatal(
      text.toShort
    ).fold(
      {l => throw new ElementDecodingFailure("SHORT", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Short = Either.catchNonFatal({
      val buffer = Unpooled.buffer(2)
      buffer.writeBytes(bytes.take(2))
      buffer.readShort
    }).fold(
      {l => throw new ElementDecodingFailure("SHORT", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Short              = throw new NullDecodedFailure("SHORT")
  }

  implicit val intElementDecoder: ElementDecoder[Int] = new ElementDecoder[Int] {
    def textDecoder(text: String): Int         = Either.catchNonFatal(
      text.toInt
    ).fold(
      {l => throw new ElementDecodingFailure("INT", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Int = Either.catchNonFatal({
      val buffer = Unpooled.buffer(4)
      buffer.writeBytes(bytes.take(4))
      buffer.readInt
    }).fold(
      {l => throw new ElementDecodingFailure("INT", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Int =            throw new NullDecodedFailure("INT")
  }

  implicit val longElementDecoder: ElementDecoder[Long] = new ElementDecoder[Long] {
    def textDecoder(text: String): Long         = Either.catchNonFatal(
      text.toLong
    ).fold(
      {l => throw new ElementDecodingFailure("LONG", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Long = Either.catchNonFatal({
      val buffer = Unpooled.buffer(8)
      buffer.writeBytes(bytes.take(8))
      buffer.readLong
    }).fold(
      {l => throw new ElementDecodingFailure("LONG", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Long              = throw new NullDecodedFailure("LONG")
  }

  implicit val floatElementDecoder: ElementDecoder[Float] = new ElementDecoder[Float] {
    def textDecoder(text: String): Float         = Either.catchNonFatal(
      text.toFloat
    ).fold(
      {l => throw new ElementDecodingFailure("FLOAT", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Float = Either.catchNonFatal({
      val buffer = Unpooled.buffer(4)
      buffer.writeBytes(bytes.take(4))
      buffer.readFloat
    }).fold(
      {l => throw new ElementDecodingFailure("FLOAT", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Float              = throw new NullDecodedFailure("FLOAT")
  }

  implicit val doubleElementDecoder: ElementDecoder[Double] = new ElementDecoder[Double] {
    def textDecoder(text: String): Double         = Either.catchNonFatal(
      text.toDouble
    ).fold(
      {l => throw new ElementDecodingFailure("DOUBLE", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Double = Either.catchNonFatal({
      val buffer = Unpooled.buffer(8)
      buffer.writeBytes(bytes.take(8))
      buffer.readDouble
    }).fold(
      {l => throw new ElementDecodingFailure("DOUBLE", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Double              = throw new NullDecodedFailure("DOUBLE")
  }

  implicit val booleanElementDecoder: ElementDecoder[Boolean] = new ElementDecoder[Boolean] {
    def textDecoder(text: String): Boolean         = Either.catchNonFatal(text.head match {
      case 't' => true
      case 'f' => false
    }).fold(
     {l => throw new ElementDecodingFailure("BOOLEAN", l)},
     {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Boolean = Either.catchNonFatal(bytes.head match {
      case 0x00 => false
      case 0x01 => true
    }).fold(
      {l => throw new ElementDecodingFailure("BOOLEAN", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Boolean              = throw new NullDecodedFailure("BOOLEAN")
  }

  implicit val charElementDecoder: ElementDecoder[Char] = new ElementDecoder[Char] {
    def textDecoder(text: String): Char         = Either.catchNonFatal(text.head.toChar).fold(
      {l => throw new ElementDecodingFailure("CHAR", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Char = Either.catchNonFatal(bytes.head.toChar).fold(
      {l => throw new ElementDecodingFailure("CHAR", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Char              = throw new NullDecodedFailure("CHAR")
  }

  implicit val jsonElementDecoder: ElementDecoder[Json] = new ElementDecoder[Json] {
    def textDecoder(text: String): Json         = Validated.fromTry(
      JParser.parseFromString(text)
    ).fold(
      {l => throw new ElementDecodingFailure("JSON", l)},
      {r => r }
    )
    def binaryDecoder(bytes: Array[Byte]): Json = Validated.fromTry({
      val buffer = ByteBuffer.wrap(bytes)
      JParser.parseFromByteBuffer(buffer)
    }).fold(
     {l => throw new ElementDecodingFailure("JSON", l)},
     {r => r}
    )
    def nullDecoder(n: Null): Json              = throw new NullDecodedFailure("JSON")
  }

  implicit val dateElementDecoders: ElementDecoder[Date] = new ElementDecoder[Date] {
    def textDecoder(text: String): Date         = Either.catchNonFatal(LocalDate.parse(text)).fold(
      {l => throw new ElementDecodingFailure("DATE", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Date = Either.catchNonFatal({
      val text = new String(bytes, StandardCharsets.UTF_8)
      LocalDate.parse(text)
    }).fold(
      {l => throw new ElementDecodingFailure("DATE", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Date              = throw new NullDecodedFailure("DATE")
  }

  implicit val localTimeElementDecoders: ElementDecoder[Time] = new ElementDecoder[Time] {
    def textDecoder(text: String): Time         = Either.catchNonFatal(LocalTime.parse(text)).fold(
      {l => throw new ElementDecodingFailure("TIME", l)},
      {r => r}
    )
    def binaryDecoder(bytes: Array[Byte]): Time = Either.catchNonFatal({
      val text = new String(bytes, StandardCharsets.UTF_8)
      LocalTime.parse(text)
    }).fold(
      {l => throw new ElementDecodingFailure("TIME", l)},
      {r => r}
    )
    def nullDecoder(n: Null): Time              = throw new NullDecodedFailure("TIME")
  }

  implicit val zonedDateTimeElementDecoders: ElementDecoder[TimestampWithTZ] = 
    new ElementDecoder[TimestampWithTZ] {
      private val zonedDateTimeFmt = new DateTimeFormatterBuilder()
        .appendPattern("yyyy-MM-dd HH:mm:ss")
        .appendFraction(ChronoField.MICRO_OF_SECOND, 0, 6, true)
        .appendOptional(DateTimeFormatter.ofPattern("X"))
        .toFormatter()
      def textDecoder(text: String): TimestampWithTZ = Either.catchNonFatal({
        ZonedDateTime.parse(text, zonedDateTimeFmt)
      }).fold(
        {l => throw new ElementDecodingFailure("TIMESTAMP WITH TIME ZONE", l)},
        {r => r}
      )
      def binaryDecoder(bytes: Array[Byte]): TimestampWithTZ = Either.catchNonFatal({
        val text = new String(bytes, StandardCharsets.UTF_8)
        ZonedDateTime.parse(text, zonedDateTimeFmt)
      }).fold(
        {l => throw new ElementDecodingFailure("TIMESTAMP WITH TIME ZONE", l)},
        {r => r}
      )
      def nullDecoder(n: Null): TimestampWithTZ = throw new NullDecodedFailure("TIMSTAMP WITH TIME ZONE")
    }
}
