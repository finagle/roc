package roc
package types

import io.netty.buffer.Unpooled
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}
import roc.postgresql.Null
import roc.types.failures.{ElementDecodingFailure, NullDecodedFailure}
import roc.types.{decoders => Decoders}

final class DecodersSpec extends Specification with ScalaCheck { def is = s2"""

  StringDecoder
    must return the correct String when Text Decoding a valid String               ${StringDecoder().testTextDecoding}
    must return the correct String when Binary Decoding a valid String Byte Array  ${StringDecoder().testBinaryDecoding}
    must throw a NullDecodedFailure when Null Decoding a String                    ${StringDecoder().testNullDecoding}

  BooleanDecoder
    must return the correct Boolean when Text Decoding a valid Boolean String               ${BooleanDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an Invalid Boolean String        ${BooleanDecoder().testInvalidTextDecoding}
    must return the correct Boolean when Binary Decoding a valid Boolean Byte Array         ${BooleanDecoder().testValidByteDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Boolean Byte Array  ${BooleanDecoder().testInvalidByteDecoding}
    must throw a NullDecodedFailure when Null Decoding an Boolean                           ${BooleanDecoder().testNullDecoding}

  OptionDecoder
    must return Some(A) when Text Decoding a valid A                       ${OptionDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an invalid A    ${OptionDecoder().testInvalidTextDecoding}
    must return Some(A) when Binary Decoding a valid A                     ${OptionDecoder().testValidBinaryDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid A  ${OptionDecoder().testInvalidBinaryDecoding}
    must return None when Null Decoding a valid A                          ${OptionDecoder().testNullDecoding}

  CharDecoder
    must return the correct Char when Text Decoding a valid String                   ${CharDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an invalid String         ${CharDecoder().testInvalidTextDecoding}
    must return the correct Char when Binary Decoding a valid Array[Byte]            ${CharDecoder().testValidBinaryDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Array[Byte]  ${CharDecoder().testInvalidBinaryDecoding}
    must throw a NullDecodedFailure when Null Decoding a Char                        ${CharDecoder().testNullDecoding}
                                                                            """

  case class StringDecoder() extends ScalaCheck {
    val testTextDecoding = forAll { x: String =>
      Decoders.stringElementDecoder.textDecoder(x) must_== x
    }
    val testBinaryDecoding = forAll { xs: Array[Byte] =>
      val expected = xs.map(_.toChar).mkString
      Decoders.stringElementDecoder.binaryDecoder(xs) must_== expected
    }
    val testNullDecoding = {
      Decoders.stringElementDecoder.nullDecoder(Null('doesnotmatter, 71)) must throwA[NullDecodedFailure]
    }
  }

  case class BooleanDecoder() extends ScalaCheck {
    val testValidTextDecoding = forAll { x: BooleanStringContainer =>
      Decoders.booleanElementDecoder.textDecoder(x.boolString) must_== x.bool
    }
    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.booleanElementDecoder.textDecoder(s) must throwA[ElementDecodingFailure]
    }
    val testValidByteDecoding = forAll { x: BooleanBytesContainer =>
      Decoders.booleanElementDecoder.binaryDecoder(x.bytes) must_== x.bool
    }
    val testInvalidByteDecoding = forAll(genInvalidBooleanBytes) { xs: Array[Byte] =>
      Decoders.booleanElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }
    val testNullDecoding = {
      Decoders.booleanElementDecoder.nullDecoder(Null('doesnotmatter, 71)) must throwA[NullDecodedFailure]
    }

    /** testValidTextDecoding */
    protected case class BooleanStringContainer(bool: Boolean, boolString: String)
    protected lazy val genBooleanStringContainer: Gen[BooleanStringContainer] = for {
      bool <- arbitrary[Boolean] 
    } yield {
      val boolString = bool match {
        case true  => "t"
        case false => "f"
      }
      new BooleanStringContainer(bool, boolString)
    }
    protected implicit lazy val arbitraryBooleanStringContainer: Arbitrary[BooleanStringContainer] =
      Arbitrary(genBooleanStringContainer)

    /** testValidBinaryDecoding */
    case class BooleanBytesContainer(bool: Boolean, bytes: Array[Byte])
    protected lazy val genValidBooleanBytesContainer: Gen[BooleanBytesContainer] = for {
      bool <- arbitrary[Boolean]
    } yield {
      val byte: Byte = bool match {
        case true  => 0x01
        case false => 0x00
      }
      new BooleanBytesContainer(bool, Array(byte))
    }
    protected implicit lazy val arbitraryBooleanBytesContainer: Arbitrary[BooleanBytesContainer] = 
      Arbitrary(genValidBooleanBytesContainer)

    /** testInvalidByteDecoding */
    protected lazy val genInvalidBooleanBytes: Gen[Array[Byte]] = for {
      byte <- arbitrary[Byte] suchThat(x => x != 0x00 && x != 0x01)
    } yield Array(byte)
  }

  case class OptionDecoder() extends ScalaCheck { 
    import roc.types.decoders._

    val testValidTextDecoding = forAll { x: IntStringContainer =>
      Decoders.optionElementDecoder[Int].textDecoder(x.intString) must_== Some(x.int)
    }
    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.optionElementDecoder[Int].textDecoder(s) must throwA[ElementDecodingFailure]
    }
    val testValidBinaryDecoding = forAll { x: IntBytesContainer =>
      Decoders.optionElementDecoder[Int].binaryDecoder(x.bytes) must_== Some(x.int)
    }
    val testInvalidBinaryDecoding = forAll(genInvalidIntBytes) { xs: Array[Byte] =>
      Decoders.optionElementDecoder[Int].binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }
    val testNullDecoding = forAll { i: Int =>
      Decoders.optionElementDecoder[Int].nullDecoder(Null('doesnotmatter, -71)) must_== None
    }

    case class IntStringContainer(int: Int, intString: String)
    protected lazy val genIntStringContainer: Gen[IntStringContainer] = for {
      int   <-  arbitrary[Int]
    } yield new IntStringContainer(int, int.toString)
    protected implicit lazy val arbitraryIntStringContainer: Arbitrary[IntStringContainer] =
      Arbitrary(genIntStringContainer)

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

    protected lazy val genInvalidIntBytes: Gen[Array[Byte]] = for {
      length    <-  Gen.oneOf(0,1,2,3)
      byte      <-  arbitrary[Byte]
    } yield Array.fill(length)(byte)
  }

  case class CharDecoder() extends ScalaCheck {

    val testValidTextDecoding = forAll { x: CharStringContainer =>
      Decoders.charElementDecoder.textDecoder(x.charString) must_== x.char
    }
    val testInvalidTextDecoding = {
      val text = ""
      Decoders.charElementDecoder.textDecoder(text) must throwA[ElementDecodingFailure]
    }
    val testValidBinaryDecoding = forAll { x: CharBytesContainer =>
      Decoders.charElementDecoder.binaryDecoder(x.bytes) must_== x.char
    }
    val testInvalidBinaryDecoding = forAll(genInvalidCharBytes) { xs: Array[Byte] =>
      Decoders.charElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }
    val testNullDecoding = {
      Decoders.charElementDecoder.nullDecoder(Null('doesnotmatter, -71)) must throwA[NullDecodedFailure]
    }

    case class CharStringContainer(char: Char, charString: String)
    protected lazy val genCharStringContainer: Gen[CharStringContainer] = for {
      char <- arbitrary[Char]
    } yield new CharStringContainer(char, char.toString)
    protected implicit lazy val arbitraryCharStringContainer: Arbitrary[CharStringContainer] = 
      Arbitrary(genCharStringContainer)

    case class CharBytesContainer(char: Char, bytes: Array[Byte])
    protected lazy val genValidCharBytesContainer: Gen[CharBytesContainer] = for {
      byte <- arbitrary[Byte]
    } yield new CharBytesContainer(byte.toChar, Array(byte))
    protected implicit lazy val arbitraryValidCharBytesContainer: Arbitrary[CharBytesContainer] =
      Arbitrary(genValidCharBytesContainer)

    protected lazy val genInvalidCharBytes: Gen[Array[Byte]] = Array.empty[Byte]
   }
}
