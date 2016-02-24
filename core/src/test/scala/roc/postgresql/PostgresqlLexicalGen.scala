package com.github.finagle
package roc
package postgresql

import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._

/** Used for generating valid Postgresql Lexical structures
  *
  * @see http://www.postgresql.org/docs/current/static/sql-syntax-lexical.html
  *     for more on what constitues a valid SQL Identifier
  */
trait PostgresqlLexicalGen extends ScalaCheck {
  // see http://www.postgresql.org/docs/current/static/sql-syntax-lexical.html
  // for more on what constitues a valid SQL Identifier
  protected val UnicodeCapitalEnglish = '\u0041' to '\u005A'
  protected val UnicodeLowerEnglish   = '\u0061' to '\u007A'
  protected val UnicodeNonLatin       = '\u0400' to '\u1FFE'
  protected val UnicodeUnderscore     = "_".getBytes(StandardCharsets.UTF_8).map(_.toChar).head
  protected val UnicodeDollarSign     = "$".getBytes(StandardCharsets.UTF_8).map(_.toChar).head
  protected val UnicodeNumbers        = '\u0030' to '\u0039'
  protected val BeginningChars = UnicodeUnderscore :: List(UnicodeCapitalEnglish, 
    UnicodeLowerEnglish, UnicodeNonLatin).flatten
  protected val SubsequentChars = UnicodeDollarSign :: BeginningChars ::: UnicodeNumbers.toList

  protected lazy val genValidBeginningIdentifier: Gen[Char] = for {
    char    <-  Gen.oneOf(BeginningChars)
  } yield char
  protected lazy val genValidSubsequentIdentifier: Gen[Char] = for {
    char    <-  Gen.oneOf(SubsequentChars)
  } yield char

  protected lazy val genValidSQLIdentifier: Gen[String] = for {
    firstChar   <-  genValidBeginningIdentifier
    chars       <-  Gen.listOf(genValidSubsequentIdentifier)
  } yield (firstChar :: chars).foldLeft("")(_ + _)

  protected lazy val genValidNumberOfShortColumns: Gen[Short] =
    arbitrary[Short] suchThat(s => s >= 0 && s < 1675) // the maximum number of columns is 1663
  protected lazy val genValidNumberOfIntColumns: Gen[Int] =
    arbitrary[Int] suchThat(n => n >= 0 && n < 1675) // the maximum number of columns is 1663
}
