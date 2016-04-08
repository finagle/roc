package roc
package types

import org.scalacheck.Prop.forAll
import org.specs2._
import roc.types.failures._

final class FailureSpec extends Specification with ScalaCheck { def is = s2"""

  BinaryDecodingUnsupportedFailure
    must have correct error message  $testBinaryDecodingFailureErrMsg

  TextDecodingUnsupportedFailure
    must have correct error message  $testTextDecodingFailureErrMsg

  NullDecodedFailure
    must have correct error message  $testNullDecodedFailureErrMsg

  ElementDecodingFailure
    must have correct error message $testElementDecodingFailureErrMsg
                                                                           """

  val testBinaryDecodingFailureErrMsg = forAll { s: String =>
    val failure = new BinaryDecodingUnsupportedFailure(s)
    failure.getMessage() must_== s"Binary decoding of type $s is currently unsupported."
  }

  val testTextDecodingFailureErrMsg = forAll { s: String =>
    val failure = new TextDecodingUnsupportedFailure(s)
    failure.getMessage() must_== s"Text decoding of type $s is currently unsupported."
  }

  val testNullDecodedFailureErrMsg = forAll { s: String =>
    val failure = new NullDecodedFailure(s)
    failure.getMessage() must_== 
      s"A NULL value was decoded for type $s. Hint: use the Option[$s] decoder, or ensure that Postgres cannot return NULL for the requested value."
  }

  val testElementDecodingFailureErrMsg = forAll { (s: String, t: Throwable) =>
    val failure = new ElementDecodingFailure(s, t)
    failure.getMessage() must_== s"Failure to decode $s. ${t.getMessage()}"
  }
}
