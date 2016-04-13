package roc
package postgresql

import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import roc.postgresql.transport.{Buf, Buffer, BufferWriter, Packet}

object generators {

  trait ErrorNoticePacketGen extends roc.postgresql.server.ErrorNoticeGen {
    private[this] type Field = (Char, String)
    private[this] type Fields = List[Field]

    private lazy val genValidErrorNoticeFieldsBuffer: Gen[(Fields, Buffer)] = for {
      fields    <-  validFieldsGen
    } yield {
      val length = fields.foldLeft(0)((i,j) => {
        i + // previous accumulation
        1 + // the one Byte Char describing the field
        Buf.lengthOfCStyleString(j._2) // the length of the C-Style String
      }) + 1 // the final null termination byte
      val buf = Buf(length)

      @annotation.tailrec
      def loop(xs: Fields): Buffer = xs match {
        case h :: t => {
          buf.writeByte(h._1.toByte)
          buf.writeCStyleString(h._2)
          loop(t)
        }
        case t => buf.writeNull; Buffer(buf.toBytes)
      }
      (fields, loop(fields))
    }

    private lazy val genInvalidErrorNoticeFieldsBuffer: Gen[(Fields, Buffer)] = for {
      xs <-  invalidFieldsGen
    } yield {
      val length = xs.foldLeft(0)((i,j) => i + 1 + lengthOfCStyleString(j._2)) + 1
      val bw = BufferWriter(new Array[Byte](length))
      xs.foreach(f => {
        bw.writeByte(f._1.toByte)
        bw.writeNullTerminatedString(f._2)
      })
      bw.writeByte(0x0)
      (xs, Buffer(bw.toBytes))
    }

    protected lazy val validErrorPacketContainerGen: Gen[ErrorNoticePacketContainer] = for {
      tuple <- genValidErrorNoticeFieldsBuffer
    } yield {
      val packet = new Packet(Some(Message.ErrorByte), tuple._2)
      new ErrorNoticePacketContainer(packet, tuple._1)
    }

    protected lazy val invalidErrorPacketContainerGen: Gen[ErrorNoticePacketContainer] = for {
      tuple <- genInvalidErrorNoticeFieldsBuffer
    } yield {
      val packet = new Packet(Some(Message.ErrorByte), tuple._2)
      new ErrorNoticePacketContainer(packet, tuple._1)
    }

    protected lazy val validNoticePacketContainerGen: Gen[ErrorNoticePacketContainer] = for {
      tuple <- genValidErrorNoticeFieldsBuffer
    } yield {
      val packet = new Packet(Some(Message.NoticeResponseByte), tuple._2)
      new ErrorNoticePacketContainer(packet, tuple._1)
    }

    protected lazy val invalidNoticePacketContainerGen: Gen[ErrorNoticePacketContainer] = for {
      tuple <- genInvalidErrorNoticeFieldsBuffer
    } yield {
      val packet = new Packet(Some(Message.NoticeResponseByte), tuple._2)
      new ErrorNoticePacketContainer(packet, tuple._1)
    }

    case class ErrorNoticePacketContainer(packet: Packet, fields: List[(Char, String)])
  }

  trait CommandCompleteGen extends ScalaCheck {

    protected lazy val genCommandString: Gen[String] = Gen.oneOf("INSERT", "DELETE", "UPDATE", 
      "SELECT", "MOVE", "FETCH", "COPY")

    protected lazy val commandCompleteValidPacket: Gen[CommandCompleteContainer] = for {
      command <-  genCommandString
      bw      =   BufferWriter(new Array[Byte](command.length + 1)).writeNullTerminatedString(command)
    } yield CommandCompleteContainer(command, 
      new Packet(Some(Message.CommandCompleteByte), Buffer(bw.toBytes)))

    case class CommandCompleteContainer(str: String, p: Packet)
  }

  trait ParameterStatusGen extends PostgresqlLexicalGen {

    // these are the standard parameters passed back, see
    // http://www.postgresql.org/docs/current/static/protocol-flow.html
    protected lazy val genParameter: Gen[String] = Gen.oneOf("server_version", "server_encoding",
      "client_encoding", "application_name", "is_superuser", "session_authorization",
      "DateStyle", "IntervalStyle", "TimeZone", "integer_datetimes", "standard_conforming_string")
    protected lazy val genParameterStatusContainer: Gen[ParameterStatusContainer] = for {
      param     <-  genParameter
      value     <-  genValidSQLIdentifier
      length    =   lengthOfCStyleStrings(param :: value :: Nil)
    } yield {
      val bw     =   BufferWriter(new Array[Byte](length))
      bw.writeNullTerminatedString(param)
      bw.writeNullTerminatedString(value)
      val packet    = new Packet(Some(Message.ParameterStatusByte), Buffer(bw.toBytes))
      new ParameterStatusContainer(param, value, packet)
    }
    implicit lazy val arbitraryPSC: Arbitrary[ParameterStatusContainer] =
      Arbitrary(genParameterStatusContainer)

    case class ParameterStatusContainer(param: String, value: String, packet: Packet)
  }

  trait BackendKeyGen extends ScalaCheck {
    protected lazy val genBackendKeyDataContainer: Gen[BackendKeyDataContainer] = for {
      processId <-  arbitrary[Int]
      secretKey <-  arbitrary[Int]
    } yield {
      val bw = BufferWriter(new Array[Byte](8))
      bw.writeInt(processId)
      bw.writeInt(secretKey)
      val packet = new Packet(Some(Message.BackendKeyDataByte), Buffer(bw.toBytes))
      new BackendKeyDataContainer(processId, secretKey, packet)
    }

    implicit lazy val arbitraryBKDC: Arbitrary[BackendKeyDataContainer] =
      Arbitrary(genBackendKeyDataContainer)

    case class BackendKeyDataContainer(processId: Int, secretKey: Int, packet: Packet)
  }

  trait ReadyForQueryGen extends ScalaCheck {

    protected lazy val validChars = 'I' :: 'T' :: 'E' :: Nil
    protected lazy val genValidChar: Gen[Char] = Gen.oneOf[Char](validChars)
    protected lazy val genInvalidChar: Gen[Byte] = 
      arbitrary[Byte] suchThat(b => !validChars.contains(b.toChar))

    protected lazy val genValidReadyForQueryContainer: Gen[ReadyForQueryContainer] = for {
      char  <-  genValidChar
    } yield new ReadyForQueryContainer(char, packetFromChar(char))
    protected lazy val genInvalidReadyForQueryContainer: Gen[ReadyForQueryContainer] = for {
      byte  <-  genInvalidChar
    } yield new ReadyForQueryContainer(byte.toChar, packetFromChar(byte.toChar)) 
    protected[this] def packetFromChar(char: Char) = {
      val bw = BufferWriter(new Array[Byte](5))
      bw.writeByte(char.toByte)
      Packet(Some(Message.ReadyForQueryByte), Buffer(bw.toBytes))
    }
  
    case class ReadyForQueryContainer(transactionStatus: Char, packet: Packet)
  }

  trait RowDescriptionGen extends FormatCodeGen with PostgresqlLexicalGen {

    protected lazy val genRowDescriptionField: Gen[RowDescriptionField] = for {
      name              <-  genValidSQLIdentifier
      tableObjectId     <-  arbitrary[Int]
      tableAttributeId  <-  arbitrary[Short]
      dataTypeObjectId  <-  arbitrary[Int]
      dataTypeSize      <-  arbitrary[Short]
      typeModifier      <-  arbitrary[Int]
      formatCode        <-  arbitrary[FormatCode]
    } yield RowDescriptionField(name, tableObjectId, tableAttributeId, dataTypeObjectId,
        dataTypeSize, typeModifier, formatCode)

    protected lazy val genRowDescriptionContainer: Gen[RowDescriptionContainer] = for {
      numFields <-  genValidNumberOfShortColumns
      fields    <-  Gen.listOfN(numFields, genRowDescriptionField)
    } yield {
      @annotation.tailrec
      def calcLength(xs: List[RowDescriptionField], length: Int): Int = xs match {
        case h :: t => {
          val fieldLength = lengthOfCStyleString(h.name) + 
            4 + // 4 Byte tableObjectId Int
            2 + // 2 Byte tableAttributeId Short
            4 + // 4 Byte dataTypeObjectId Int
            2 + // 2 Byte dataTypeSize Short
            4 + // 4 Byte typeModifier Int
            2   // 2 Byte formatCode Short
            calcLength(t, length + fieldLength)
        }
        case t      => length
      }
      val fieldsLength = calcLength(fields, 0)
      val length = fieldsLength + 2 // 2 Byte short for number of fields
      val bw = BufferWriter(new Array[Byte](length))
      bw.writeShort(numFields)
      fields.foreach(f => {
        bw.writeNullTerminatedString(f.name, StandardCharsets.UTF_8)
        bw.writeInt(f.tableObjectId)
        bw.writeShort(f.tableAttributeId)
        bw.writeInt(f.dataTypeObjectId)
        bw.writeShort(f.dataTypeSize)
        bw.writeInt(f.typeModifier)
        f.formatCode match {
          case TextFormat   => bw.writeShort(0.toShort)
          case BinaryFormat => bw.writeShort(1.toShort)
        }
      })
      val packet = Packet(Some(Message.RowDescriptionByte), Buffer(bw.toBytes))
      new RowDescriptionContainer(numFields, fields, packet)
    }

    protected lazy val genUnknownFormatCodeRDC: Gen[RowDescriptionFormatCodeContainer] = for {
      numFields     <-  genValidNonZeroNumberOfShortColumns
      fields        <-  Gen.listOfN(numFields, genRowDescriptionField)
      formatCode    <-  genUnknownFormatCode
    } yield {
      @annotation.tailrec
      def calcLength(xs: List[RowDescriptionField], length: Int): Int = xs match {
        case h :: t => {
          val fieldLength = lengthOfCStyleString(h.name) + 
            4 + // 4 Byte tableObjectId Int
            2 + // 2 Byte tableAttributeId Short
            4 + // 4 Byte dataTypeObjectId Int
            2 + // 2 Byte dataTypeSize Short
            4 + // 4 Byte typeModifier Int
            2   // 2 Byte formatCode Short
            calcLength(t, length + fieldLength)
        }
        case t      => length
      }
      val fieldsLength = calcLength(fields, 0)
      val length = fieldsLength + 2 // 2 Byte short for number of fields
      val bw = BufferWriter(new Array[Byte](length))
      //val formatCode = 3.toShort
      bw.writeShort(numFields)
      fields.foreach(f => {
        bw.writeNullTerminatedString(f.name, StandardCharsets.UTF_8)
        bw.writeInt(f.tableObjectId)
        bw.writeShort(f.tableAttributeId)
        bw.writeInt(f.dataTypeObjectId)
        bw.writeShort(f.dataTypeSize)
        bw.writeInt(f.typeModifier)
        bw.writeShort(formatCode)
      })
      val packet = Packet(Some(Message.RowDescriptionByte), Buffer(bw.toBytes))
      new RowDescriptionFormatCodeContainer(numFields, fields, packet, formatCode)
    }

    protected lazy val genUnknownFormatCode: Gen[Short] =
      Gen.chooseNum[Short](2, Short.MaxValue)

    case class RowDescriptionContainer(numFields: Short, fields: List[RowDescriptionField], 
      packet: Packet)

    case class RowDescriptionFormatCodeContainer(numFields: Short, fields: List[RowDescriptionField], 
      packet: Packet, formatCode: Short)
  }

  trait DataRowGen extends PostgresqlLexicalGen {
    protected lazy val genColumnBytes: Gen[Option[Array[Byte]]] = arbitrary[Option[Array[Byte]]]
    protected lazy val genDataRowContainer: Gen[DataRowContainer] = for {
      columns   <-  genValidNumberOfShortColumns
      bytes     <-  Gen.listOfN(columns, genColumnBytes)
    } yield {
      val calcBytesLength = (column: Option[Array[Byte]]) => column match {
        case None     => 0
        case Some(ab) => ab.length 
      }
      val addLengthOfColumn: (Int) => Int = (bytesLength: Int) => bytesLength + 4
      val calcLengthOfColumn: (Option[Array[Byte]]) => Int =
        calcBytesLength andThen addLengthOfColumn

      @annotation.tailrec
      def lengthOfColumns(as: List[Option[Array[Byte]]], length: Int): Int = as match {
        case h :: t => lengthOfColumns(t, length + calcLengthOfColumn(h))
        case t      => length
      }

      val totalLength = lengthOfColumns(bytes, 0) + 2 // 2 Byte short for number of columns
      val bw = BufferWriter(new Array[Byte](totalLength + 1))
      bw.writeShort(columns)

      @annotation.tailrec
      def writeColumns(xs: List[Option[Array[Byte]]]): Unit = xs match {
        case h :: t => {
          h match {
            case Some(b) => {
              bw.writeInt(b.length)
              bw.writeBytes(b)
            }
            case None    => bw.writeInt(-1)
          }
          writeColumns(t)
        }
        case t => 
      }

      val _ = writeColumns(bytes)
      val packet = Packet(Some(Message.DataRowByte), Buffer(bw.toBytes))
      new DataRowContainer(columns, bytes, packet)
    }

    protected lazy implicit val arbitraryDataRowContainer: Arbitrary[DataRowContainer] =
      Arbitrary(genDataRowContainer)

    case class DataRowContainer(numColumns: Short, columnBytes: List[Option[Array[Byte]]], 
      packet: Packet)
  }

  trait AuthenticationMessagesGen extends ScalaCheck {
    case class AuthMessageContainer(requestType: Int, salt: Array[Byte], authBytes: Array[Byte],
      packet: Packet)

    private val knownMessageInts = 0 :: 2 :: 3 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
    protected lazy val genAuthMessageInt: Gen[Int] = Gen.oneOf(knownMessageInts)
    protected lazy val genUnknownMessageInt: Gen[Int] =
      arbitrary[Int] suchThat(n => !knownMessageInts.contains(n))
    protected lazy val genByte: Gen[Byte] = arbitrary[Byte]
    protected lazy val genSalt: Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](4, genByte)
    protected lazy val genAuthBytes: Gen[Array[Byte]] = Gen.containerOf[Array, Byte](genByte)

    protected lazy val genValidAuthMessageContainer: Gen[AuthMessageContainer] = for {
      requestType   <-  genAuthMessageInt
      salt          <-  genSalt
      authBytes     <-  genAuthBytes
    } yield buildContainer(requestType, salt, authBytes)
    protected lazy val genUnknownReqAuthMessageContainer: Gen[AuthMessageContainer] = for {
      requestType   <-  genUnknownMessageInt
      salt          <-  genSalt
      authBytes     <-  genAuthBytes
    } yield buildContainer(requestType, salt, authBytes)

    private[this] def buildContainer(requestType: Int, salt: Array[Byte], 
      authBytes: Array[Byte]): AuthMessageContainer = {
        val length = if(requestType == 5) {
          8 
        } else if(requestType == 8) {
          authBytes.length + 4
        } else {
          4
        }
        val bw = BufferWriter(new Array[Byte](length))
        bw.writeInt(requestType)
        if(requestType == 5) {
          bw.writeBytes(salt)
        } else if(requestType == 8) {
          bw.writeBytes(authBytes)
        }

        val packet = new Packet(Some(Message.AuthenticationMessageByte), Buffer(bw.toBytes))
        new AuthMessageContainer(requestType, salt, authBytes, packet)
      }
  }

  trait FormatCodeGen extends ScalaCheck {
    protected lazy val genFormatCode: Gen[FormatCode] = Gen.oneOf(TextFormat, BinaryFormat)

    implicit lazy val arbitraryFormatCode: Arbitrary[FormatCode] =
      Arbitrary(genFormatCode)
  }
}
