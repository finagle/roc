package com.github.finagle
package roc
package postgresql

import cats.data.Xor
import com.github.finagle.roc.postgresql.transport.{Buffer, BufferWriter, Packet}
import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import org.specs2.specification.core._
import org.specs2.specification.create.FragmentsFactory

final class PacketDecodersSpec extends Specification with ScalaCheck { def is = s2"""

  ErrorMessage
    should return a PacketDecodingFailure("Error Messages not implemented yet")     ${ErrorMsg().test}

  CommandComplete
    should return Xor.Right(CommandComplete) when given a valid Packet              ${CmdComplete().test}
    should return Xor.Left(PacketDecodingFailure) when given an invalid Packet      ${CmdComplete().testInvalidPacket}

  ParameterStatus
    should return Xor.Right(ParameterStatus) when given a valid Packet              ${ParamStatus().test}
    should return Xor.Left(PacketDecodingFailure) when given an invalid Packet      ${ParamStatus().testInvalidPacket}

  BackendKeyData
    should return Xor.Right(BackendKeyData) when given a valid Packet               ${BackendKey().test}
    should return Xor.Left(PacketDecodingFailure) when given an invalid Packet      ${BackendKey().testInvalidPacket}

  ReadyForQuery
    should return Xor.Right(ReadyForQuery) when given a valid Char                  ${RFQ().testValid}
    should return Xor.Left(ReadyForQueryDecodingFailure) when given an invalid Char ${RFQ().testInvalidChar}
    should return Xor.Left(PacketDecodingFailure) when given an invalid Packet      ${RFQ().testInvalidPacket}

  RowDescription
    should return Xor.Right(RowDescription) when given a valid Packet               ${RD().testValidPacket}
    should return Xor.Left(PacketDecodingFailure) when given an invalid Packet      ${RD().testInvalidPacket}

  DataRow
    should return Xor.Right(DataRow) when given a valid Packet                      ${DR().testValidPacket}
    should return Xor.Left(PacketDecodingFailure) when given an invalid Packet      ${DR().testInvalidPacket}
                                                                                  """

  case class ErrorMsg() extends generators.ErrorGen {
    val test = forAll(errorPacket) { (p: Packet) => 
      decodePacket[ErrorMessage](p) must_== 
        Xor.Left(new PacketDecodingFailure("Error messages not implemented yet"))
    }
  }

  case class CmdComplete() extends generators.CommandCompleteGen {
    val test = forAll(commandCompleteValidPacket) { (c: CommandCompleteContainer) =>
      decodePacket[CommandComplete](c.p) must_== Xor.Right(new CommandComplete(c.str))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.CommandCompleteByte), Buffer(Array.empty[Byte]))
      decodePacket[CommandComplete](packet) must_== 
        Xor.Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }

  case class ParamStatus() extends generators.ParameterStatusGen {
    val test = forAll { (psc: ParameterStatusContainer) =>
      decodePacket[ParameterStatus](psc.packet) must_== 
        Xor.Right(new ParameterStatus(psc.param, psc.value))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.CommandCompleteByte), Buffer(Array.empty[Byte]))
      decodePacket[ParameterStatus](packet) must_== 
        Xor.Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }

  case class BackendKey() extends generators.BackendKeyGen {
    val test = forAll { (bkdc: BackendKeyDataContainer) =>
      val backendKeyData = new BackendKeyData(bkdc.processId, bkdc.secretKey)
      decodePacket[BackendKeyData](bkdc.packet) must_== Xor.Right(backendKeyData)
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.CommandCompleteByte), Buffer(Array.empty[Byte]))
      decodePacket[BackendKeyData](packet) must_== 
        Xor.Left(new PacketDecodingFailure("Not enough readable bytes - Need 4, maximum is 0"))
    }
  }

  case class RFQ() extends generators.ReadyForQueryGen {
    val testValid = forAll(genValidReadyForQueryContainer) { (rfqc: ReadyForQueryContainer) =>
      rfqc.transactionStatus match {
        case 'I' => decodePacket[ReadyForQuery](rfqc.packet) must_== Xor.Right(Idle)
        case 'T' => decodePacket[ReadyForQuery](rfqc.packet) must_== Xor.Right(TransactionBlock)
        case 'E' => decodePacket[ReadyForQuery](rfqc.packet) must_== Xor.Right(FailedTransactionBlock)
        case  c  => decodePacket[ReadyForQuery](rfqc.packet) must_==
          Xor.Left(new ReadyForQueryDecodingFailure(rfqc.transactionStatus))
      }
    }

    val testInvalidChar = forAll(genInvalidReadyForQueryContainer) { (rfqc: ReadyForQueryContainer) =>
      decodePacket[ReadyForQuery](rfqc.packet) must_==
        Xor.Left(new ReadyForQueryDecodingFailure(rfqc.transactionStatus))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.ReadyForQueryByte), Buffer(Array.empty[Byte]))
      decodePacket[ReadyForQuery](packet) must_== 
        Xor.Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }

  case class RD() extends generators.RowDescriptionGen {

    val testValidPacket = forAll(genRowDescriptionContainer) { (rdc: RowDescriptionContainer) => 
      decodePacket[RowDescription](rdc.packet) must_==
        Xor.Right(RowDescription(rdc.numFields, rdc.fields))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.RowDescriptionByte), Buffer(Array.empty[Byte]))
      decodePacket[RowDescription](packet) must_==
        Xor.Left(new PacketDecodingFailure("Not enough readable bytes - Need 2, maximum is 0"))
    }
  }

  case class DR() extends generators.DataRowGen {
    val testValidPacket = forAll { (drc: DataRowContainer) =>
      val dr = decodePacket[DataRow](drc.packet)
      decodePacket[DataRow](drc.packet) must_==
        Xor.Right(DataRow(drc.numColumns, drc.columnBytes))
    }

    val testInvalidPacket = { 
      val packet = Packet(Some(Message.DataRowByte), Buffer(Array.empty[Byte]))
      decodePacket[DataRow](packet) must_==
        Xor.Left(new PacketDecodingFailure("Not enough readable bytes - Need 2, maximum is 0"))
    }
  }
}
