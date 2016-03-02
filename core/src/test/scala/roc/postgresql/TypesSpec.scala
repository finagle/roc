package roc
package postgresql

import cats.data.Xor
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.specs2._
import org.specs2.specification.core._

final class TypesSpec extends Specification with ScalaCheck { def is = s2"""

  PostgresType
    should create Int4                                         $int4
    should create VarChar                                      $varChar
    should create TimestampWithTimezone                        $timestampWithTimezone
    should create UnknownPostgresTypeFailure for invalid type  $unknownOID
                                                                         """
  def int4 = PostgresType(23) must_== Xor.Right(Int4)
  def varChar = PostgresType(1043) must_== Xor.Right(VarChar)
  def timestampWithTimezone = PostgresType(1184) must_== Xor.Right(TimestampWithTimezone)

  val unknownOID = forAll(unknownOIDInt) { n =>
    val error = new UnknownPostgresTypeFailure(n)
    PostgresType(n) must_== Xor.Left(error)
  }

  lazy val unknownOIDInt: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)
    .suchThat(!PostgresType.oids.contains(_))
}

