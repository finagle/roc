package roc
package types

import io.circe.generic.auto._
import io.circe.syntax._
import java.nio.charset.StandardCharsets
import jawn.ast.JParser
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}
import roc.types.failures.{ElementDecodingFailure, NullDecodedFailure}
import roc.types.{decoders => Decoders}

final class JsonDecoderSpec extends Specification with ScalaCheck { def is = s2"""

  JsonDecoder
    must correctly decode Text representation                              $testValidText
    must throw a ElementDecodingFailure when Text decoding invalid Json    $testInvalidText
    must correctly decode Binary representation                            $testValidBinary
    must throw a ElementDecodingFailure when Binary decoding invalid Json  $testInvalidBinary
    must throw a NullDecodedFailure when Null decoding Json                $testNullDecoding

                                                                               """

  private val testValidText = forAll { x: JsonContainer =>
    Decoders.jsonElementDecoder.textDecoder(x.text) must_== x.json
  }

  private val testInvalidText = forAll { x: String =>
    Decoders.jsonElementDecoder.textDecoder(x) must throwA[ElementDecodingFailure]
  }

  private val testValidBinary = forAll { x: BinaryJsonContainer =>
    Decoders.jsonElementDecoder.binaryDecoder(x.binary) must_== x.json
  }

  private val testInvalidBinary = forAll { xs: Array[Byte] =>
    Decoders.jsonElementDecoder.binaryDecoder(xs) must throwA[ElementDecodingFailure]
  }

  private val testNullDecoding = 
    Decoders.jsonElementDecoder.nullDecoder must throwA[NullDecodedFailure]

  case class JsonContainer(text: String, json: Json)
  private lazy val genJsonContainer: Gen[JsonContainer] = for {
    jObject <- arbitrary[JsonObject]
  } yield {
    val text = jObject.asJson.noSpaces
    val json = JParser.parseUnsafe(text)
    new JsonContainer(text, json)
  }
  private implicit lazy val arbitraryJsonContainer: Arbitrary[JsonContainer] = 
    Arbitrary(genJsonContainer)

  case class BinaryJsonContainer(binary: Array[Byte], json: Json)
  private lazy val genBinaryJsonContainer: Gen[BinaryJsonContainer] = for {
    jObject <- arbitrary[JsonObject]
  } yield {
    val text = jObject.asJson.noSpaces
    val json = JParser.parseUnsafe(text)
    val bytes = text.getBytes(StandardCharsets.UTF_8)
    new BinaryJsonContainer(bytes, json)
  }
  private implicit lazy val arbitraryBinaryJsonContainer: Arbitrary[BinaryJsonContainer] =
    Arbitrary(genBinaryJsonContainer)

  case class JsonObject(name: String, first_names: List[String])

  private lazy val genJsonObject: Gen[JsonObject] = for {
    name <- arbitrary[String]
    first_names <- arbitrary[List[String]]
  } yield new JsonObject(name, first_names)
  private implicit lazy val arbitraryJsonObject: Arbitrary[JsonObject] = 
    Arbitrary(genJsonObject)
}
