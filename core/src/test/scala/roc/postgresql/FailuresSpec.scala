package roc
package postgresql

import cats.data.NonEmptyList
import cats.implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.specification.core._
import roc.postgresql.failures._

final class FailuresSpec extends Specification with ScalaCheck { def is = s2"""

  Failure
    UnknownPostgresTypeFailure should have correct message           $unknownPostgresTypeFailure
    ReadyForQueryDecodingFailure should have correct message         $readyForQueryDecodingFailure
    UnknownAuthenticationFailure should have correct message         $unknownAuthRequestFailure
    UnsupportedAuthenticationFailure should have correct message     $unsupportedAuthFailure
    PostgresqlStateMachineFailure should have correct message        $postgresqlStateMachineFailure
    UnknownPostgresqlMessageTypeFailure should have correct message  $unknownPostgresqlMessageTypeFailure
    PostgresqlMessageDecodingFailure must have a correct error message   $postgresqlMessageDecodingFailure
                                                                         """

  val unknownPostgresTypeFailure = forAll { n: Int =>
    val msg = s"Postgres Object ID $n is unknown"
    val error = new UnknownPostgresTypeFailure(n)
    error.getMessage must_== msg
  }

  val readyForQueryDecodingFailure = forAll { c: Char =>
    val msg = s"Received unexpected Char $c from Postgres Server."
    val error = new ReadyForQueryDecodingFailure(c)
    error.getMessage must_== msg
  }

  val unknownAuthRequestFailure = forAll { n: Int =>
    val expectedMessage = s"Unknown Authentication Request Type: $n"
    val error           = new UnknownAuthenticationRequestFailure(n)
    error.getMessage must_== expectedMessage
  }

  val unsupportedAuthFailure = forAll { s: String =>
    val expectedMessage =
      s"Unsupported Authentication Failure. $s authentication is not supported."
    val error           = new UnsupportedAuthenticationFailure(s)
    error.getMessage must_== expectedMessage
  }
  
  val postgresqlStateMachineFailure = forAll { (s1: String, s2: String) =>
    val expectedMessage = s"State Transition from $s1 -> $s2 is undefined."
    val error           = new PostgresqlStateMachineFailure(s1, s2)
    error.getMessage must_== expectedMessage
  }
  
  val unknownPostgresqlMessageTypeFailure = forAll { c: Char =>
    val expectedMessage = s"Unknown Postgresql MessageType '$c'."
    val error           = new UnknownPostgresqlMessageTypeFailure(c)
    error.getMessage must_== expectedMessage
  }

  val postgresqlMessageDecodingFailure = forAll(genNELErrorResponse) { nel: NonEmptyList[String] =>
    val error = new PostgresqlMessageDecodingFailure(nel)
    val expectedMessage = nel.foldLeft("")(_ + _)
    expectedMessage must_== error.getMessage
  }

  private lazy val genErrorResponseString: Gen[String] = Gen.oneOf(
    "Required Severity Level was not present.",
    "Required SQLSTATE Code was not present.",
    "Required Message was not present."
  )
  private lazy val genNELErrorResponse: Gen[NonEmptyList[String]] = for {
    string  <-  genErrorResponseString
  } yield NonEmptyList(string)

}

