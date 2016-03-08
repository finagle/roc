package roc
package postgresql
package server

import cats.data.Xor
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.specs2._

final class WarningSpec extends Specification with ErrorNoticeGen { def is = s2"""

  Warning
    must return Xor.Right(Warning(ErrorParams)) when SQLSTATE Code is '01' $testWarning
                                                                                            """

  val testWarning = forAll(warningGen) { x: FieldsAndErrorParams =>
    PostgresqlError(x.fields) must_== Xor.Right(Warning(x.errorParams))
  }

  private lazy val warningGen: Gen[FieldsAndErrorParams] = for {
    severity      <-  genValidSeverityField
    message       <-  arbitrary[String]
    optional      <-  genOptionalFields
  } yield {
    val fields = buildFields(severity, ErrorClassCodes.Warning, message, optional)
    val e = buildErrorParamsFromFields(fields)
    new FieldsAndErrorParams(fields, e)
  }

}
