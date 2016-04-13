package roc
package types

import org.scalacheck.Prop.forAll
import org.specs2._
import roc.types.failures._
import roc.types.{decoders => Decoders}

final class DecodersSpec extends Specification with ScalaCheck { def is = s2"""

  StringDecoder
    must return the correct String when Text Decoding a valid String               ${StringDecoder().testTextDecoding}
    must return the correct String when Binary Decoding a valid String Byte Array  ${StringDecoder().testBinaryDecoding}
    must throw a NullDecodedFailure when Null Decoding a String                    ${StringDecoder().testNullDecoding}

  IntDecoder
    must return the correct Int when Text Decoding a valid Int String                   ${IntDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an Invalid Int String        ${IntDecoder().testInvalidTextDecoding}
    must return the correct Int when Binary Decoding a valid Int Byte Array             ${IntDecoder().testValidByteDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid Int Byte Array  ${IntDecoder().testInvalidByteDecoding}
    must throw a NullDecodedFailure when Null Decoding an Int                           ${IntDecoder().testNullDecoding}

  OptionDecoder
    must return Some(A) when Text Decoding a valid A                       ${OptionDecoder().testValidTextDecoding}
    must throw a ElementDecodingFailure when Text Decoding an invalid A    ${OptionDecoder().testInvalidTextDecoding}
    must return Some(A) when Binary Decoding a valid A                     ${OptionDecoder().testValidBinaryDecoding}
    must throw a ElementDecodingFailure when Binary Decoding an invalid A  ${OptionDecoder().testInvalidBinaryDecoding}
    must return None when Null Decoding a valid A                          ${OptionDecoder().testNullDecoding}

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
      Decoders.stringElementDecoder.nullDecoder must throwA[NullDecodedFailure]
    }
  }

  case class IntDecoder() extends generators.IntDecoderGen {
    val testValidTextDecoding = forAll(genValidInt) { x: IntContainer =>
      Decoders.intElementDecoder.textDecoder(x.strValue) must_== x.int
    }
    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.intElementDecoder.textDecoder(s) must throwA[ElementDecodingFailure]
    }
    val testValidByteDecoding = forAll(genValidIntBytes) { x: IntBytesContainer =>
      Decoders.intElementDecoder.binaryDecoder(x.bytes) must_== x.int
    }
    val testInvalidByteDecoding = forAll(genInvalidIntBytes) { xs: Array[Byte] =>
      Decoders.intElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }
    val testNullDecoding = {
      Decoders.intElementDecoder.nullDecoder must throwA[NullDecodedFailure]
    }
  }

  case class OptionDecoder() extends generators.OptionDecoderGen {
    import roc.types.decoders._ 

    val testValidTextDecoding = forAll(genValidInt) { x: IntContainer =>
      Decoders.optionElementDecoder[Int].textDecoder(x.strValue) must_== Some(x.int)
    }
    val testInvalidTextDecoding = forAll { s: String =>
      Decoders.optionElementDecoder[Int].textDecoder(s) must throwA[ElementDecodingFailure]
    }
    val testValidBinaryDecoding = forAll(genValidIntBytes) { x: IntBytesContainer =>
      Decoders.optionElementDecoder[Int].binaryDecoder(x.bytes) must_== Some(x.int)
    }
    val testInvalidBinaryDecoding = forAll(genInvalidIntBytes) { xs: Array[Byte] =>
      Decoders.optionElementDecoder[Int].binaryDecoder(xs) must throwA[ElementDecodingFailure]
    }
    val testNullDecoding = forAll { i: Int =>
      Decoders.optionElementDecoder[Int].nullDecoder() must_== None
    }
  }
}
