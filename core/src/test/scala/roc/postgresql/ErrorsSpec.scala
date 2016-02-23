package com.github.finagle
package roc
package postgresql

import org.scalacheck.Prop.forAll
import org.specs2._
import org.specs2.specification.core._

final class ErrorsSpec extends Specification with ScalaCheck { def is = s2"""

  Error
    UnknownPostgresTypeFailure should have correct message  $unkownPostgresTypeFailure
                                                                         """
  val unkownPostgresTypeFailure = forAll { n: Int =>
    val msg = s"Postgres Object ID $n is unknown"
    val error = new UnknownPostgresTypeFailure(n)
    error.getMessage must_== msg
  }
}

