package com.github.finagle
package roc
package postgresql

import java.nio.charset.StandardCharsets
import org.scalacheck.Prop.forAll
import org.specs2._

final class MessagesSpec extends Specification with ScalaCheck { def is = s2"""

  Message
    should calculate length of C-Style String               $lengthOfCStyleString
                                                            """

  val lengthOfCStyleString = forAll { (str: String) =>
    val bytes  = str.getBytes(StandardCharsets.UTF_8)
    val length = bytes.length + 1 // add 1 for null character
    Message.lengthOfCStyleString(str) must_== length
  }
}
