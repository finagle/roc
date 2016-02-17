package com.github.finagle
package roc
package postgresql

import com.github.finagle.roc.postgresql.transport.{Buffer, BufferWriter, Packet}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.specification.core._
import org.specs2.specification.create.FragmentsFactory

final class AuthenticationMessageSpec extends Specification with ScalaCheck { def is = s2"""

  PasswordMessage
    should encode starting with 'p'                 $pmEncodeMessageType
    should encode a valid Packet Body               $pmEncodeBody
    should MD5 encrypt a password with given salt   $pmEncrypt

  StartupMessage
    should calculate the length of Body Byte Array  $lengthOfByteArray
    should encode a valid Packet Message Type       $smPacketEncodeMessageType
    should encode a valid Packet Body               $smPacketEncodeBody
                                                           """
  val pmEncodeMessageType = forAll { (pm: PasswordMessage) => 
    pm.encode.messageType must_== Some('p')
  }

  val pmEncodeBody = forAll { (pm: PasswordMessage) =>
    val length = pm.password.getBytes(StandardCharsets.UTF_8).length
    val bw     = BufferWriter(new Array[Byte](length + 1))
    bw.writeNullTerminatedString(pm.password)
    val packet = Packet(Some('p'), Buffer(bw.toBytes))
    pm.encode.body.underlying must_== packet.body.underlying
  }

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

  val lengthOfByteArray = forAll { (sm: StartupMessage) =>
    val shorts = 4
    val i = Message.lengthOfCStyleString("user")
    val j = Message.lengthOfCStyleString(sm.user)
    val k = Message.lengthOfCStyleString("database")
    val l = Message.lengthOfCStyleString(sm.database)
    val lastNull = 1
    val total = shorts + i + j + k + l + lastNull
    StartupMessage.lengthOfByteArray(sm) must_== total
  }

  val smPacketEncodeMessageType = forAll { (sm: StartupMessage) =>
    sm.encode.messageType must_== None
  }

  val smPacketEncodeBody = forAll { (sm: StartupMessage) =>
    val buffer = BufferWriter(new Array[Byte](StartupMessage.lengthOfByteArray(sm)))
    buffer.writeShort(3)
    buffer.writeShort(0)
    buffer.writeNullTerminatedString("user")
    buffer.writeNullTerminatedString(sm.user)
    buffer.writeNullTerminatedString("database")
    buffer.writeNullTerminatedString(sm.database)
    buffer.writeNull
    val packet = Packet(None, Buffer(buffer.toBytes))

    sm.encode.body.underlying must_== packet.body.underlying
  }

  lazy val genPasswordMessage: Gen[PasswordMessage] = for {
    password    <-  arbitrary[String]
  } yield new PasswordMessage(password)
  implicit lazy val implicitPasswordMessage: Arbitrary[PasswordMessage] = 
    Arbitrary(genPasswordMessage)

  lazy val genByte: Gen[Byte] = arbitrary[Byte]
  lazy val genSalt: Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](4, genByte)

  lazy val genStartupMessage: Gen[StartupMessage] = for {
    username    <-  arbitrary[String]
    database    <-  arbitrary[String]
  } yield new StartupMessage(username, database)
  implicit lazy val implicitStartupMessage: Arbitrary[StartupMessage] = 
    Arbitrary(genStartupMessage)

  override val ff = fragmentFactory
}
