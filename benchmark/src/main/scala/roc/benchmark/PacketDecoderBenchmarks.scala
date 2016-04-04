package roc.postgresql.transport

import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import roc.postgresql.Message
import roc.postgresql.server.ErrorClassCodes._
import roc.postgresql.server.ErrorNoticeMessageFields
import roc.postgresql.server.ErrorNoticeMessageFields._

@Fork(2)
@State(Scope.Thread)
class PacketDecoderBenchmarks extends TestData {

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureReadErrorNoticeThroughput(): Unit = {
    Decoders.readErrorNoticePacket(readErrorNoticePacket)
    ()
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureRowDescriptionPacketDecoder(): Unit = {
    Decoders.rowDescriptionPacketDecoder(rowDescriptionPacket)
    ()
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureDataRowPacketDecoder(): Unit = {
    Decoders.dataRowPacketDecoder(dataRowPacket)
    ()
  }
}


abstract class TestData {

  object Decoders extends PacketDecoderImplicits

  protected val readErrorNoticePacket: Packet = {
    val xs: List[(Char, String)] = List((Severity, "FATAL"), (Code, DataException), (
      ErrorNoticeMessageFields.Message, "Unknown table \"FOO\""), (Detail, "An amazing detail"),
      (Hint, "A super cool hint"), (Position, "L31"), (InternalPosition, "L89"), (InternalQuery,
      "Your internaly Query was AWFUL"), (Where, "I don't know, somewhere?"), (SchemaName,
      "SCHEMANAME"), (TableName, "the_best_table_ever"), (ColumnName, "last_updated_at"),
      (DataTypeName, "Int"), (ConstraintName, "id_constraint"), (File, "postgres.c"), (Line,
      "71"), (Routine, "the_best_routine"))

    val length = xs.foldLeft(0)((i,j) => i + 1 + lengthOfCStyleString(j._2)) + 1
    val bw = BufferWriter(new Array[Byte](length))
    xs.foreach(f => {
      bw.writeByte(f._1.toByte)
      bw.writeNullTerminatedString(f._2)
    })
    bw.writeByte(0x0)
    Packet(Some(Message.ErrorByte), Buffer(bw.toBytes))
  }

  protected val rowDescriptionPacket: Packet = {
    import roc.postgresql.RowDescriptionField
    import roc.postgresql.{BinaryFormat, TextFormat}

    val field = new RowDescriptionField(name = "the_greatest_column_name", tableObjectId = 7171,
      tableAttributeId = 71.toShort, dataTypeObjectId = 7171, dataTypeSize = 71.toShort,
      typeModifier = 7171, formatCode = TextFormat)
    val fields = List.fill(100)(field)

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
      bw.writeShort(100.toShort)
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
    Packet(Some(Message.RowDescriptionByte), Buffer(bw.toBytes))
  }

  protected val dataRowPacket: Packet = {
    val columnBytes = Array.fill(773)(0x31.toByte)
    val columns = Array.fill(100)(columnBytes)

    val columnLength = columnBytes.length + 4 
    val totalLength = (columnLength * 100) + 2 // 2 Byte short for number of columns
    val bw = BufferWriter(new Array[Byte](totalLength + 1))
    bw.writeShort(100.toShort)

    @annotation.tailrec
    def writeColumns(xs: List[Array[Byte]]): Unit = xs match {
      case h :: t => {
        bw.writeInt(h.length)
        bw.writeBytes(h)
        writeColumns(t)
      }
      case t => 
    }

    val _ = writeColumns(columns.toList)
    Packet(Some(Message.DataRowByte), Buffer(bw.toBytes))
  }

  private def lengthOfCStyleString(str: String): Int = {
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    bytes.length + 1
  }
}
