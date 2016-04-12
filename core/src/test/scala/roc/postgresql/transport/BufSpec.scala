package roc
package postgresql
package transport

import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.specification.core._

final class BufSpec extends Specification with ScalaCheck { def is = s2"""

  Buf
    must calculate the length of a C-Style string          $testCStyleString
    must calculate the length of a list of C-Style strings $testCStyleStrings

    must read and write a byte    $testByte
    must read and write bytes     $testBytes
    must read and write a string  $testStrings
    must read and write a short   $testShort
    must read and write an int    $testInts
    must read and write a long    $testLongs
    must read and write a float   $testFloat
    must read and write a double  $testDouble
    must covert to byte array     $testToBytes
    
  """

  val testCStyleString = forAll { s: String =>
    val expected = s.getBytes(StandardCharsets.UTF_8).length + 1
    Buf.lengthOfCStyleString(s) must_== expected
  }

  val testCStyleStrings = forAll { xs: List[String] =>
    val lengths = xs.map(x => Buf.lengthOfCStyleString(x))
    var length = 0
    lengths.foreach(x => length += x)

    Buf.lengthOfCStyleStrings(xs) must_== length
  }

  val testByte = forAll { b: Byte =>
    val buf = Buf(1)
    buf.writeByte(b)
    val byte = buf.readByte

    byte must_== b
  }

  val testBytes = forAll { xs: Array[Byte] =>
    val buf = Buf(xs.length)
    buf.writeBytes(xs)
    val bytes     = buf.readBytes(xs.length)
    val sameElems = bytes sameElements xs

    sameElems must_== true
  }

  val testShort = forAll { s: Short => 
    val buf = Buf(2)
    buf.writeShort(s)
    val short = buf.readShort

    short must_== s
  }

  val testInts = forAll { i: Int =>
    val buf = Buf(4)
    buf.writeInt(i)
    val int = buf.readInt

    int must_== i
  }

  val testStrings = forAll(genNonZeroLengthString) { s: String =>
    val length = Buf.lengthOfCStyleString(s)
    val buf = Buf(length)
    buf.writeCStyleString(s)
    val str = buf.readCStyleString()

    str must_== s
  }

  val testLongs = forAll { l: Long =>
    val buf = Buf(8)
    buf.writeLong(l)
    val long = buf.readLong

    long must_== l
  }

  val testFloat = forAll { f: Float =>
    val buf = Buf(4)
    buf.writeFloat(f)
    val float = buf.readFloat

    float must_== f
  }

  val testDouble = forAll { d: Double =>
    val buf = Buf(8)
    buf.writeDouble(d)
    val double = buf.readDouble

    double must_== d
  }

  val testToBytes = forAll { xs: Array[Byte] =>
    val buf = Buf(xs)
    val bytes = buf.toBytes

    val sameBytes = bytes sameElements xs
    sameBytes must_== true
  }

  lazy val genNonZeroLengthString: Gen[String] = arbitrary[String] suchThat(_.length > 0)
}
