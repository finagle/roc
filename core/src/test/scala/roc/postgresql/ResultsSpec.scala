package roc
package postgresql

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.mock.Mockito
import org.specs2.specification.core._
import roc.postgresql.failures.ElementNotFoundFailure

final class ResultsSpec extends Specification with ScalaCheck with Mockito { def is = s2"""

  Row
    get(column) must throw ElementNotFound failure for unknown column name  $columnNotFound
                                                                           """
  
  val columnNotFound = forAll { sym: Symbol =>
    val row = new Row(List.empty[Element])
    row.get(sym) must throwA[ElementNotFoundFailure]
  }

  lazy val genSymbol: Gen[Symbol] = for {
    str <-  arbitrary[String]
  } yield Symbol(str)
  implicit lazy val arbitrarySymbol: Arbitrary[Symbol] =
    Arbitrary(genSymbol)
}
