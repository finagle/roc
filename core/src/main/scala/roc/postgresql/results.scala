package com.github.finagle
package roc
package postgresql

import cats.data.Xor
import cats.Show
import com.github.finagle.roc.postgresql.transport.BufferReader
import com.twitter.util.Future

final class Result(rowDescription: RowDescription, data: List[DataRow]) {

  val columns =  rowDescription.fields
    .map(x => Symbol(x.name))
    .zip(rowDescription.fields)
    .map(tuple => {
      val postgresType = PostgresType(tuple._2.dataTypeObjectId) match {
        case Xor.Right(t) => t
        case Xor.Left(l)  => throw l
      }
      new Column(tuple._1, postgresType, tuple._2.formatCode)
    })

  val rows = data.map(x => new Row(columns, x))
}

final case class Column(name: Symbol, columnType: PostgresType, formatCode: FormatCode) {
  final override def toString: String = Column.columnShow.show(this)
}
object Column {

  implicit val columnShow: Show[Column] = new Show[Column] {
    def show(c: Column): String = 
      s"Column(name=${c.name}, columnType=${c.columnType}, formatCode=${c.formatCode})"
  }
}


final class Row(private[postgresql] val columns: List[Column], dataRow: DataRow) {
  object `package` extends postgresql.ByteDecoderImplicits

  def get[A](columnName: Symbol)(implicit f: ByteDecoder[A]): A = {
    val idx = indexOfColumn(columnName)
    val column = columns(idx)
    column.formatCode match {
      case Text   => f.fromText(dataRow.columnBytes(idx)) match {
        case Xor.Right(r) => r match {
          case Some(x)    => x
          case None       => throw new UnexpectedNoneFailure("Got Option when None was expected")
        }
        case Xor.Left(l)  => throw l
      }
      case Binary => f.fromBinary(dataRow.columnBytes(idx)) match {
        case Xor.Right(r) => r match {
          case Some(x)    => x
          case None       => throw new UnexpectedNoneFailure("Got Option when None was expected")
        }
        case Xor.Left(l)  => throw l
      }
    }
  }

  private[postgresql] def indexOfColumn(sym: Symbol): Int = 
    columns.indexWhere(_.name == sym) match {
      case -1   => throw new ColumnNotFoundException(sym)
      case n    => n
    }

  final override def toString: String = Row.rowShow.show(this)
}
object Row {
  implicit val rowShow: Show[Row] = new Show[Row] {
    def show(r: Row): String = 
      s"Row(columns=${r.columns})"
  }
}

