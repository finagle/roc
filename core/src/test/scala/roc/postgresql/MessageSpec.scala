package com.github.finagle
package roc
package postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._

final class MessagesSpec extends Specification with ScalaCheck { def is = s2"""

  PasswordMessage
    should MD5 encrypt a password with given salt           $pmEncrypt
                                                                            """

  val pmEncrypt = forAll { (user: String, pm: PasswordMessage, salt: Array[Byte]) =>
    val md = MessageDigest.getInstance("MD5")
    md.update((pm.password+ user).getBytes(StandardCharsets.UTF_8))
    val unsaltedHexStr = md.digest().map(x => "%02x".format(x.byteValue)).foldLeft("")(_ + _)
    val saltedBytes = unsaltedHexStr.getBytes ++ salt
    md.reset()
    md.update(saltedBytes)
    val passwd = md.digest().map(x => "%02x".format(x.byteValue)).foldLeft("md5")(_ + _)
    passwd must_== PasswordMessage.encryptMD5Passwd(user, pm.password, salt)
  }
  
  lazy val genByte: Gen[Byte] = arbitrary[Byte]
  lazy val genSalt: Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](4, genByte)
  lazy val genPasswordMessage: Gen[PasswordMessage] = for {
    password    <-  arbitrary[String]
  } yield new PasswordMessage(password)
  implicit lazy val implicitPasswordMessage: Arbitrary[PasswordMessage] = 
    Arbitrary(genPasswordMessage)
}
