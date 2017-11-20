package roc
package postgresql
package transport

import org.scalacheck.Prop.forAll
import org.specs2.{ScalaCheck, Specification}
import roc.postgresql.failures.{PacketDecodingFailure, ReadyForQueryDecodingFailure,
  UnknownAuthenticationRequestFailure}
import roc.postgresql.server.PostgresqlMessage

final class PacketDecodersSpec extends Specification with ScalaCheck { def is = s2"""

  ErrorMessage
    must return Right(ErrorMessage(PostgresqlMessage)) when given a valid Packet        ${ErrorMsg().test}
    must return Left(ErrorResponseDecodingFailure) when given an invalid Error Message  ${ErrorMsg().testInvalid}
    must return Left(PacketDecodingFailure) when given an invalid Packet                ${ErrorMsg().testInvalidPacket}

  CommandComplete
    should return Right(CommandComplete) when given a valid Packet              ${CmdComplete().test}
    should return Left(PacketDecodingFailure) when given an invalid Packet      ${CmdComplete().testInvalidPacket}

  ParameterStatus
    should return Right(ParameterStatus) when given a valid Packet              ${ParamStatus().test}
    should return Left(PacketDecodingFailure) when given an invalid Packet      ${ParamStatus().testInvalidPacket}

  BackendKeyData
    should return Right(BackendKeyData) when given a valid Packet               ${BackendKey().test}
    should return Left(PacketDecodingFailure) when given an invalid Packet      ${BackendKey().testInvalidPacket}

  ReadyForQuery
    should return Right(ReadyForQuery) when given a valid Char                  ${RFQ().testValid}
    should return Left(ReadyForQueryDecodingFailure) when given an invalid Char ${RFQ().testInvalidChar}
    should return Left(PacketDecodingFailure) when given an invalid Packet      ${RFQ().testInvalidPacket}

  DataRow
    should return Right(DataRow) when given a valid Packet                      ${DR().testValidPacket}
    should return Left(PacketDecodingFailure) when given an invalid Packet      ${DR().testInvalidPacket}

  RowDescription
    should return Right(RowDescription) when given a valid Packet               ${RD().testValidPacket}
    should return Left(PacketDecodingFailure) when given an invalid Packet      ${RD().testInvalidPacket}
    should have valid Message when decoding an unknown Format Code                  ${RD().testUnknownFormatCode}

  AuthenticationMessages
    should return Right(AuthenticationMessage) when given a valid Message Int              ${AM().testValid}
    should return Left(UnknownAuthenticationRequestFailure) when given an Unknown Request  ${AM().testUnkownRequestType}
    should return Left(PacketDecodingFailure) when given an invalid Packet                 ${AM().testInvalidPacket}

  NoticeResponseMessage
    must return Right(NoticeResponse(PostgresqlMessage)) when given a valid Packet             ${NR().test}
    must return Left(PostgresqlMessageDecodingFailure) when given an invalid PostgresqlMessage ${NR().testInvalid}
    must return Left(PacketDecodingFailure) when given an invalid Packet                       ${NR().testInvalidPacket}

                                                                                  """

  case class ErrorMsg() extends generators.ErrorNoticePacketGen {
    val test = forAll(validErrorPacketContainerGen) { c: ErrorNoticePacketContainer =>
      val message = PostgresqlMessage(c.fields).getOrElse(throw new Exception("Generator Failed"))
      decodePacket[ErrorResponse](c.packet) must_== Right(ErrorResponse(message))
    }

    val testInvalid = forAll(invalidErrorPacketContainerGen) { c: ErrorNoticePacketContainer =>
      decodePacket[ErrorResponse](c.packet) must_== PostgresqlMessage(c.fields)
    }

    val testInvalidPacket = {
      val packet = Packet(Some(Message.CommandCompleteByte), Buffer(Array.empty[Byte]))
      decodePacket[ErrorResponse](packet) must_==
        Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }

  case class CmdComplete() extends generators.CommandCompleteGen {
    val test = forAll(commandCompleteValidPacket) { (c: CommandCompleteContainer) =>
      decodePacket[CommandComplete](c.p) must_== Right(new CommandComplete(c.str))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.CommandCompleteByte), Buffer(Array.empty[Byte]))
      decodePacket[CommandComplete](packet) must_== 
        Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }

  case class ParamStatus() extends generators.ParameterStatusGen {
    val test = forAll { (psc: ParameterStatusContainer) =>
      decodePacket[ParameterStatus](psc.packet) must_== 
        Right(new ParameterStatus(psc.param, psc.value))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.CommandCompleteByte), Buffer(Array.empty[Byte]))
      decodePacket[ParameterStatus](packet) must_== 
        Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }

  case class BackendKey() extends generators.BackendKeyGen {
    val test = forAll { (bkdc: BackendKeyDataContainer) =>
      val backendKeyData = new BackendKeyData(bkdc.processId, bkdc.secretKey)
      decodePacket[BackendKeyData](bkdc.packet) must_== Right(backendKeyData)
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.CommandCompleteByte), Buffer(Array.empty[Byte]))
      decodePacket[BackendKeyData](packet) must_== 
        Left(new PacketDecodingFailure("Not enough readable bytes - Need 4, maximum is 0"))
    }
  }

  case class RFQ() extends generators.ReadyForQueryGen {
    val testValid = forAll(genValidReadyForQueryContainer) { (rfqc: ReadyForQueryContainer) =>
      rfqc.transactionStatus match {
        case 'I' => decodePacket[ReadyForQuery](rfqc.packet) must_== Right(Idle)
        case 'T' => decodePacket[ReadyForQuery](rfqc.packet) must_== Right(TransactionBlock)
        case 'E' => decodePacket[ReadyForQuery](rfqc.packet) must_== Right(FailedTransactionBlock)
        case  c  => decodePacket[ReadyForQuery](rfqc.packet) must_==
          Left(new ReadyForQueryDecodingFailure(rfqc.transactionStatus))
      }
    }

    val testInvalidChar = forAll(genInvalidReadyForQueryContainer) { (rfqc: ReadyForQueryContainer) =>
      decodePacket[ReadyForQuery](rfqc.packet) must_==
        Left(new ReadyForQueryDecodingFailure(rfqc.transactionStatus))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.ReadyForQueryByte), Buffer(Array.empty[Byte]))
      decodePacket[ReadyForQuery](packet) must_== 
        Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }

  case class RD() extends generators.RowDescriptionGen {

    val testValidPacket = forAll(genRowDescriptionContainer) { (rdc: RowDescriptionContainer) => 
      decodePacket[RowDescription](rdc.packet) must_==
        Right(RowDescription(rdc.numFields, rdc.fields))
    }

    def testInvalidPacket = {
      val packet = Packet(Some(Message.RowDescriptionByte), Buffer(Array.empty[Byte]))
      decodePacket[RowDescription](packet) must_==
        Left(new PacketDecodingFailure("Not enough readable bytes - Need 2, maximum is 0"))
    }

    val testUnknownFormatCode = 
      forAll(genUnknownFormatCodeRDC) { (rdc: RowDescriptionFormatCodeContainer) =>
        decodePacket[RowDescription](rdc.packet) must_==
          Left(new PacketDecodingFailure(s"Unknown format code ${rdc.formatCode}."))
      }
  }

  case class DR() extends generators.DataRowGen {
    val testValidPacket = forAll { x: DataRowContainer  =>
      decodePacket[DataRow](x.packet) must_== Right(x.dataRow)
    }

    val testInvalidPacket = { 
      val packet = Packet(Some(Message.DataRowByte), Buffer(Array.empty[Byte]))
      decodePacket[DataRow](packet) must_==
        Left(new PacketDecodingFailure("Not enough readable bytes - Need 2, maximum is 0"))
    }
  }

  case class AM() extends generators.AuthenticationMessagesGen {
    val testValid = forAll(genValidAuthMessageContainer) { (amc: AuthMessageContainer) =>
      val decodedPacket = decodePacket[AuthenticationMessage](amc.packet)
      amc.requestType match {
        case 0  => decodedPacket must_== Right(AuthenticationOk)
        case 2  => decodedPacket must_== Right(AuthenticationKerberosV5)
        case 3  => decodedPacket must_== Right(AuthenticationClearTxtPasswd)
        case 5  => decodedPacket must_== Right(new AuthenticationMD5Passwd(amc.salt))
        case 6  => decodedPacket must_== Right(AuthenticationSCMCredential)
        case 7  => decodedPacket must_== Right(AuthenticationGSS)
        case 8  => decodedPacket must_== Right(new AuthenticationGSSContinue(amc.authBytes))
        case 9  => decodedPacket must_== Right(AuthenticationSSPI)
        case x  => decodedPacket must_==
          Left(new UnknownAuthenticationRequestFailure(amc.requestType))
      }
    }

    val testUnkownRequestType = 
      forAll(genUnknownReqAuthMessageContainer) { (amc: AuthMessageContainer) =>
        decodePacket[AuthenticationMessage](amc.packet) must_==
         Left(new UnknownAuthenticationRequestFailure(amc.requestType))
      }

    val testInvalidPacket = {
      val packet = Packet(Some(Message.AuthenticationMessageByte), Buffer(Array.empty[Byte]))
      decodePacket[AuthenticationMessage](packet) must_==
        Left(new PacketDecodingFailure("Not enough readable bytes - Need 4, maximum is 0"))
    }
  }

  case class NR() extends generators.ErrorNoticePacketGen {
    val test = forAll(validNoticePacketContainerGen) { c: ErrorNoticePacketContainer =>
      val message = PostgresqlMessage(c.fields).getOrElse(throw new Exception("Generator Failed"))
      decodePacket[NoticeResponse](c.packet) must_== Right(NoticeResponse(message))
    }

    val testInvalid = forAll(invalidNoticePacketContainerGen) { c: ErrorNoticePacketContainer =>
      decodePacket[NoticeResponse](c.packet) must_== PostgresqlMessage(c.fields)
    }

    val testInvalidPacket = {
      val packet = Packet(Some(Message.NoticeResponseByte), Buffer(Array.empty[Byte]))
      decodePacket[NoticeResponse](packet) must_==
        Left(new PacketDecodingFailure("Readable byte limit exceeded: 0"))
    }
  }
}
