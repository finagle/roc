package roc
package postgresql
package transport

import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.specification.core._
import org.specs2.specification.create.FragmentsFactory

final class PacketEncodersSpec extends Specification with ScalaCheck { def is = s2"""

  PasswordMessage
    should encode starting with 'p'                 $pmEncodeMessageType
    should encode a valid Packet Body               $pmEncodeBody

  StartupMessage
    should calculate the length of Body Byte Array  $lengthOfByteArray
    should encode a valid Packet Message Type       $smPacketEncodeMessageType
    should encode a valid Packet Body               $smPacketEncodeBody

  QueryMessage
    should encode starting with 'Q'                 $qEncodeMessageType
    should encode a valid Packet Body               $qEncodeBody

  TerminateMessage
    should encode starting with 'X'                 $tEncodeMessageType
                                                                                  """

  val pmEncodeMessageType = forAll { (pm: PasswordMessage) => 
    encodePacket(pm).messageType must_== Some('p')
  }

  val pmEncodeBody = forAll { (pm: PasswordMessage) =>
    val length = pm.password.getBytes(StandardCharsets.UTF_8).length
    val bw     = BufferWriter(new Array[Byte](length + 1))
    bw.writeNullTerminatedString(pm.password)
    val packet = Packet(Some(Message.PasswordMessageByte), Buffer(bw.toBytes))

    encodePacket(pm).body.underlying must_== packet.body.underlying
  }

  val lengthOfByteArray = forAll { (sm: StartupMessage) =>
    val shorts   = 4
    val length   = lengthOfCStyleStrings( "user" :: sm.user :: "database" :: sm.database :: Nil)
    val lastNull = 1
    val total = shorts + length + lastNull

    PacketEncoder.lengthOfStartupMessageByteArray(sm) must_== total
  }

  val smPacketEncodeMessageType = forAll { (sm: StartupMessage) =>
    encodePacket(sm).messageType must_== None
  }

  val smPacketEncodeBody = forAll { (sm: StartupMessage) =>
    val buffer = BufferWriter(new Array[Byte](PacketEncoder.lengthOfStartupMessageByteArray(sm)))
    buffer.writeShort(3)
    buffer.writeShort(0)
    buffer.writeNullTerminatedString("user")
    buffer.writeNullTerminatedString(sm.user)
    buffer.writeNullTerminatedString("database")
    buffer.writeNullTerminatedString(sm.database)
    buffer.writeNull
    val packet = Packet(None, Buffer(buffer.toBytes))

    encodePacket(sm).body.underlying must_== packet.body.underlying
  }

  val qEncodeMessageType = forAll{ (q: Query) => 
    encodePacket(q).messageType must_== Some('Q')
  }

  val qEncodeBody = forAll{ (q: Query) => 
    val length = q.queryString.getBytes(StandardCharsets.UTF_8).length
    val bw     = BufferWriter(new Array[Byte](length + 1))
    bw.writeNullTerminatedString(q.queryString)
    val bytes  = bw.toBytes
    val packet = Packet(Some(Message.QueryMessageByte), Buffer(bytes))

    encodePacket(q).body.underlying must_== packet.body.underlying
  }

  val tEncodeMessageType = forAll { t: Terminate =>
    encodePacket(t).messageType must_== Some('X')
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

  lazy val genQuery: Gen[Query] = for {
    queryString <-  arbitrary[String]
  } yield new Query(queryString)
  implicit lazy val implicitQuery: Arbitrary[Query] = 
    Arbitrary(genQuery)

  lazy val genTerminate: Gen[Terminate] = new Terminate()
  implicit lazy val implicitTerminate: Arbitrary[Terminate] = Arbitrary(genTerminate)

  override val ff = fragmentFactory
}
