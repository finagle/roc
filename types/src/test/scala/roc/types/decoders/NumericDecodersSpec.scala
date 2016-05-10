package roc
package types

import io.netty.buffer.Unpooled
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import roc.types.failures.{ElementDecodingFailure, NullDecodedFailure}
import roc.types.{decoders => Decoders}

final class NumericDecodersSpec extends Specification with ScalaCheck { def is = s2"""

  Short Decoder
    must return the correct Short when Text Decoding a valid Short String                 ${ShortDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when TextDecoding an Invalid Short String         ${ShortDecoder().testInvalidTextDecoding}
    must return the correct Short when Binary Decoding a valid Short Byte Array           ${ShortDecoder().testValidBinaryDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Short Byte Array  ${ShortDecoder().testInvalidBinaryDecoding}
    must throw a NullDecodedFailure when Null Decoding a Short                            ${ShortDecoder().testNullDecoding}

  Int Decoder
    must return the correct Int when Text Decoding a valid Int String                   ${IntDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an Invalid Int String        ${IntDecoder().testInvalidTextDecoding}
    must return the correct Int when Binary Decoding a valid Int Byte Array             ${IntDecoder().testValidByteDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Int Byte Array  ${IntDecoder().testInvalidByteDecoding}
    must throw a NullDecodedFailure when Null Decoding an Int                           ${IntDecoder().testNullDecoding}

  Long Decoder
    must return the correct Long when Text Decoding a valid Long String                  ${LongDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an Invalid Long String        ${LongDecoder().testInvalidTextDecoding}
    must return the correct Long when Binary Decoding a valid Long Byte Array            ${LongDecoder().testValidByteDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Long Byte Array  ${LongDecoder().testInvalidByteDecoding}
    must throw a NullDecodedFailure when Null Decoding a Long                            ${LongDecoder().testNullDecoding}

  Float Decoder
    must return the correct Float when Text Decoding a valid Float String                 ${FloatDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an Invalid Float String        ${FloatDecoder().testInvalidTextDecoding}
    must return the correct Float when Binary Decoding a valid Float Byte Array           ${FloatDecoder().testValidByteDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Float Byte Array  ${FloatDecoder().testInvalidByteDecoding}
    must throw a NullDecodedFailure when Null Decoding a Float                            ${FloatDecoder().testNullDecoding}

  Double Decoder
    must return the correct Double when Text Decoding a valid Double String                ${DoubleDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an Invalid Double String        ${DoubleDecoder().testInvalidTextDecoding}
    must return the correct Double when Binary Decoding a valid Double Byte Array          ${DoubleDecoder().testValidByteDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Double Byte Array  ${DoubleDecoder().testInvalidByteDecoding}
    must throw a NullDecodedFailure when Null Decoding a Double                            ${DoubleDecoder().testNullDecoding}
                                                                            """

  case class ShortDecoder() extends ScalaCheck {

    val testValidTextDecoding = forAll { x: ShortStringContainer =>
      Decoders.shortElementDecoder.textDecoder(x.shortString) must_== x.short
    }

    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.shortElementDecoder.textDecoder(s) must throwA[ElementDecodingFailure]
    }

    val testValidBinaryDecoding = forAll { x: ShortBytesContainer =>
      Decoders.shortElementDecoder.binaryDecoder(x.bytes) must_== x.short
    }

    val testInvalidBinaryDecoding = forAll(genInvalidShortBytes) { xs: Array[Byte] =>
      Decoders.shortElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }

    val testNullDecoding = Decoders.shortElementDecoder.nullDecoder must throwA[NullDecodedFailure]

    /** testValidTextDecoding */
     case class ShortStringContainer(short: Short, shortString: String)
     protected lazy val genValidShortStringContainer: Gen[ShortStringContainer] = for {
       s    <-  arbitrary[Short]
     } yield new ShortStringContainer(s, s.toString)
     protected implicit lazy val arbitraryShortStringContainer: Arbitrary[ShortStringContainer] =
       Arbitrary(genValidShortStringContainer)

    /** testValidBinaryDecoding */
    case class ShortBytesContainer(short: Short, bytes: Array[Byte])
    protected lazy val genValidShortBytesContainer: Gen[ShortBytesContainer] = for {
      short <-  arbitrary[Short]
    } yield {
      val buffer = Unpooled.directBuffer(2)
      buffer.writeShort(short)
      val xs = Array.fill[Byte](2)(0x00)
      buffer.readBytes(xs)
      new ShortBytesContainer(short, xs)
    }
    protected implicit lazy val arbitraryShortBytesContainer: Arbitrary[ShortBytesContainer] = 
      Arbitrary(genValidShortBytesContainer)

    /** testInvalidByteDecoding */
    protected lazy val genInvalidShortBytes: Gen[Array[Byte]] = for {
      count <-  Gen.oneOf(0,1)
      byte  <-  arbitrary[Byte]
    } yield Array.fill(count)(byte)
  }

  case class IntDecoder() extends ScalaCheck {

    val testValidTextDecoding = forAll { x: IntStringContainer =>
      Decoders.intElementDecoder.textDecoder(x.strValue) must_== x.int
    }

    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.intElementDecoder.textDecoder(s) must throwA[ElementDecodingFailure]
    }

    val testValidByteDecoding = forAll { x: IntBytesContainer =>
      Decoders.intElementDecoder.binaryDecoder(x.bytes) must_== x.int
    }

    val testInvalidByteDecoding = forAll(genInvalidIntBytes) { xs: Array[Byte] =>
      Decoders.intElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }

    val testNullDecoding = Decoders.intElementDecoder.nullDecoder must throwA[NullDecodedFailure]

    /** testValidTextDecoding */
    protected case class IntStringContainer(int: Int, strValue: String)
    protected lazy val genValidIntStringContainer: Gen[IntStringContainer] = for {
      int   <-  arbitrary[Int]
    } yield new IntStringContainer(int, int.toString)
    protected implicit lazy val arbitraryIntStringContainer: Arbitrary[IntStringContainer] =
      Arbitrary(genValidIntStringContainer)

    /** testValidBinaryDecoding */
    case class IntBytesContainer(int: Int, bytes: Array[Byte])
    protected lazy val genValidIntBytesContainer: Gen[IntBytesContainer] = for {
      int   <-  arbitrary[Int]
    } yield {
      val buffer = Unpooled.directBuffer(4)
      buffer.writeInt(int)
      val xs = Array.fill[Byte](4)(0x00)
      buffer.readBytes(xs)
      new IntBytesContainer(int, xs)
    }
    protected implicit lazy val arbitraryIntBytesContainer: Arbitrary[IntBytesContainer] = 
      Arbitrary(genValidIntBytesContainer)

    /** testInvalidByteDecoding */
    protected lazy val genInvalidIntBytes: Gen[Array[Byte]] = for {
      length    <-  Gen.oneOf(0,1,2,3)
      byte      <-  arbitrary[Byte]
    } yield Array.fill(length)(byte)
  }

  case class LongDecoder() extends ScalaCheck {
    val testValidTextDecoding = forAll { x: LongStringContainer =>
      Decoders.longElementDecoder.textDecoder(x.longString) must_== x.long
    }

    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.longElementDecoder.textDecoder(s) must throwA[ElementDecodingFailure]
    }

    val testValidByteDecoding = forAll { x: LongBytesContainer =>
      Decoders.longElementDecoder.binaryDecoder(x.bytes) must_== x.long
    }

    val testInvalidByteDecoding = forAll(genInvalidLongBytes) { xs: Array[Byte] =>
      Decoders.longElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }

    val testNullDecoding = Decoders.longElementDecoder.nullDecoder must throwA[NullDecodedFailure]

    /** testValidTextDecoding */
    protected case class LongStringContainer(long: Long, longString: String)
    protected lazy val genValidLongStringContainer: Gen[LongStringContainer] = for {
      long  <-  arbitrary[Long]
    } yield new LongStringContainer(long, long.toString)
    protected implicit lazy val arbitraryLongStringContainer: Arbitrary[LongStringContainer] =
      Arbitrary(genValidLongStringContainer)

    /** testValidBinaryDecoding */
    case class LongBytesContainer(long: Long, bytes: Array[Byte])
    protected lazy val genLongBytesContainer: Gen[LongBytesContainer] = for {
      long  <-  arbitrary[Long]
    } yield {
      val buffer = Unpooled.directBuffer(8)
      buffer.writeLong(long)
      val xs = Array.fill[Byte](8)(0x00)
      buffer.readBytes(xs)
      new LongBytesContainer(long, xs)
    }
    protected implicit lazy val arbitraryLongBytesContainer: Arbitrary[LongBytesContainer] = 
      Arbitrary(genLongBytesContainer)

    /** testInvalidByteDecoding */
    protected lazy val genInvalidLongBytes: Gen[Array[Byte]] = for {
      length    <-  Gen.oneOf(0,1,2,3,4,5,6,7)
      byte      <-  arbitrary[Byte]
    } yield Array.fill(length)(byte)
  }

  case class FloatDecoder() extends ScalaCheck {
    val testValidTextDecoding = forAll { x: FloatStringContainer =>
      Decoders.floatElementDecoder.textDecoder(x.floatString) must_== x.float
    }

    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.floatElementDecoder.textDecoder(s) must throwA[ElementDecodingFailure]
    }

    val testValidByteDecoding = forAll { x: FloatBytesContainer =>
      Decoders.floatElementDecoder.binaryDecoder(x.bytes) must_== x.float
    }

    val testInvalidByteDecoding = forAll(genInvalidFloatBytes) { xs: Array[Byte] =>
      Decoders.floatElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }

    val testNullDecoding = Decoders.floatElementDecoder.nullDecoder must throwA[NullDecodedFailure]

    /** testValidTextDecoding */
    protected case class FloatStringContainer(float: Float, floatString: String)
    protected lazy val genFloatStringContainer: Gen[FloatStringContainer] = for {
      float <-  arbitrary[Float]
    } yield new FloatStringContainer(float, float.toString)
    protected implicit lazy val arbitraryFloatStringContainer: Arbitrary[FloatStringContainer] =
      Arbitrary(genFloatStringContainer)

    /** testValidBinaryDecoding */
    case class FloatBytesContainer(float: Float, bytes: Array[Byte])
    protected lazy val genFloatBytesContainer: Gen[FloatBytesContainer] = for {
      float <-  arbitrary[Float]
    } yield {
      val buffer = Unpooled.directBuffer(4)
      buffer.writeFloat(float)
      val xs = Array.fill[Byte](4)(0x00)
      buffer.readBytes(xs)
      new FloatBytesContainer(float, xs)
    }
    protected implicit lazy val arbitraryFloatBytesContainer: Arbitrary[FloatBytesContainer] = 
      Arbitrary(genFloatBytesContainer)

    /** testInvalidByteDecoding */
    protected lazy val genInvalidFloatBytes: Gen[Array[Byte]] = for {
      length    <-  Gen.oneOf(0,1,2,3)
      byte      <-  arbitrary[Byte]
    } yield Array.fill(length)(byte)
  }

  case class DoubleDecoder() extends ScalaCheck {
    val testValidTextDecoding = forAll { x: DoubleStringContainer =>
      Decoders.doubleElementDecoder.textDecoder(x.doubleString) must_== x.double
    }

    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.doubleElementDecoder.textDecoder(s) must throwA[ElementDecodingFailure]
    }

    val testValidByteDecoding = forAll { x: DoubleBytesContainer =>
      Decoders.doubleElementDecoder.binaryDecoder(x.bytes) must_== x.double
    }

    val testInvalidByteDecoding = forAll(genInvalidDoubleBytes) { xs: Array[Byte] =>
      Decoders.doubleElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }

    val testNullDecoding = Decoders.doubleElementDecoder.nullDecoder must throwA[NullDecodedFailure]

    /** testValidTextDecoding */
    protected case class DoubleStringContainer(double: Double, doubleString: String)
    protected lazy val genDoubleStringContainer: Gen[DoubleStringContainer] = for {
      double    <-  arbitrary[Double]
    } yield new DoubleStringContainer(double, double.toString)
    protected implicit lazy val arbitraryDoubleStringContainer: Arbitrary[DoubleStringContainer] =
      Arbitrary(genDoubleStringContainer)

    /** testValidBinaryDecoding */
    case class DoubleBytesContainer(double: Double, bytes: Array[Byte])
    protected lazy val genDoubleBytesContainer: Gen[DoubleBytesContainer] = for {
      double    <-  arbitrary[Double]
    } yield {
      val buffer = Unpooled.directBuffer(8)
      buffer.writeDouble(double)
      val xs = Array.fill[Byte](8)(0x00)
      buffer.readBytes(xs)
      new DoubleBytesContainer(double, xs)
    }
    protected implicit lazy val arbitraryDoubleBytesContainer: Arbitrary[DoubleBytesContainer] = 
      Arbitrary(genDoubleBytesContainer)

    /** testInvalidByteDecoding */
    protected lazy val genInvalidDoubleBytes: Gen[Array[Byte]] = for {
      length    <-  Gen.oneOf(0,1,2,3,4,5,6,7)
      byte      <-  arbitrary[Byte]
    } yield Array.fill(length)(byte)
  }
}
