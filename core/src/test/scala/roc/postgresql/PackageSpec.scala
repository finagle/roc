package com.github.finagle
package roc
package postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._

final class PackageSpec extends Specification with ScalaCheck { def is = s2"""

  Postgresql Package
    should calculate length of C-Style String               $test0
    should calculate length of C-Style Strings              $test1
                                                                           """

  val test0 = forAll { (str: String) => 
    val bytes  = str.getBytes(StandardCharsets.UTF_8)
    val length = bytes.length + 1 // add 1 for null character
    lengthOfCStyleString(str) must_== length
  }

  val test1 = forAll { (xs: List[String]) => 
    val length = xs match {
      case h :: t => xs.map(lengthOfCStyleString).reduce(_ + _)
      case t      => 0
    }
    lengthOfCStyleStrings(xs) must_== length
  }

}
