package roc
package types

import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import roc.postgresql.transport.BufferWriter
import roc.types.failures._
import roc.types.{decoders => Decoders}

object generators {

  abstract class IntDecoderGen extends ScalaCheck {
    protected lazy val genValidInt: Gen[IntContainer] = for {
      int   <-  arbitrary[Int]
    } yield new IntContainer(int, int.toString)
    case class IntContainer(int: Int, strValue: String)

    protected lazy val genValidIntBytes: Gen[IntBytesContainer] = for {
      int   <-  arbitrary[Int]
    } yield {
      val bw = BufferWriter(new Array[Byte](4))
      bw.writeInt(int)
      new IntBytesContainer(int, bw.toBytes)
    }
    case class IntBytesContainer(int: Int, bytes: Array[Byte])

    protected lazy val genInvalidIntBytes: Gen[Array[Byte]] = for {
      length    <-  Gen.oneOf(0,1,2,3)
      byte      <-  arbitrary[Byte]
    } yield Array.fill(length)(byte)
  }

  def lengthOfCStyleString(s: String): Int = {
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    bytes.length + 1
  }

  abstract class OptionDecoderGen extends IntDecoderGen
}
