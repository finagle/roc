package com.github.finagle
package roc
package postgresql

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.mock.Mockito
import org.specs2.specification.core._

final class ResultsSpec extends Specification with ScalaCheck with Mockito { def is = s2"""

  Row
    get[A](column) should throw ColumnNotFoundException for unknown column  $columnNotFound
                                                                           """
  
  val columnNotFound = forAll { sym: Symbol =>
    val m = mock[DataRow]
    val row = new Row(List.empty[Column], m)
    row.get[String](sym) must throwA[ColumnNotFoundException]
  }

  lazy val genSymbol: Gen[Symbol] = for {
    str <-  arbitrary[String]
  } yield Symbol(str)
  implicit lazy val arbitrarySymbol: Arbitrary[Symbol] =
    Arbitrary(genSymbol)
}                                                  
