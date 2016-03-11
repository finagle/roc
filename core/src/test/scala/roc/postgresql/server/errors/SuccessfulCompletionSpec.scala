package roc
package postgresql
package server

import cats.data.Xor
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.specs2._

final class SuccessfulCompletionSpec extends Specification with ErrorNoticeGen { def is = s2"""

  SuccessfulCompletion
    must return Xor.Right(SuccessfulCompletion(ErrorParams)) when SQLSTATE Code is '00' $testSuccessfulCompletion
                                                                                            """

  val testSuccessfulCompletion = forAll(successfulCompletionGen) { x: FieldsAndErrorParams =>
    PostgresqlError(x.fields) must_== Xor.Right(SuccessfulCompletion(x.errorParams))
  }

  private lazy val successfulCompletionGen: Gen[FieldsAndErrorParams] = for {
    severity      <-  genValidSeverityField
    message       <-  arbitrary[String]
    optional      <-  genOptionalFields
  } yield {
    val fields = buildFields(severity, ErrorClassCodes.SuccessfulCompletion, message, optional)
    val e = buildErrorParamsFromFields(fields)
    new FieldsAndErrorParams(fields, e)
  }

}
