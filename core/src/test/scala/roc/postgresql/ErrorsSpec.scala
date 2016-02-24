package com.github.finagle
package roc
package postgresql

import org.scalacheck.Prop.forAll
import org.specs2._
import org.specs2.specification.core._

final class ErrorsSpec extends Specification with ScalaCheck { def is = s2"""

  Error
    UnknownPostgresTypeFailure should have correct message    $unkownPostgresTypeFailure
    ReadyForQueryDecodingFailure should have correct message  $readyForQueryDecodingFailure

                                                                         """
  val unkownPostgresTypeFailure = forAll { n: Int =>
    val msg = s"Postgres Object ID $n is unknown"
    val error = new UnknownPostgresTypeFailure(n)
    error.getMessage must_== msg
  }

  val readyForQueryDecodingFailure = forAll { c: Char =>
    val msg = s"Received unexpected Char $c from Postgres Server."
    val error = new ReadyForQueryDecodingFailure(c)
    error.getMessage must_== msg
  }
}

