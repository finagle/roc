package roc
package postgresql

import cats.data.Xor
import cats.Show
import roc.postgresql.failures.{ElementNotFoundFailure, UnsupportedDecodingFailure}
import roc.postgresql.server.PostgresqlMessage

final class Result(rowDescription: List[RowDescription], data: List[DataRow], cc: String = "")
  extends Iterable[Row] {

  val columns = rowDescription match {
    case h :: t => h.fields
      .map(x => Symbol(x.name))
      .zip(h.fields)
      .map(x => new Column(x._1, x._2.dataTypeObjectId, x._2.formatCode))
      .toArray
    case t => Array.empty[Column]
  }

  private[this] val rows: List[Row] = data.map(x => {
    @annotation.tailrec
    def loop(xs: List[Option[Array[Byte]]], ys: List[Element], i: Int): List[Element] = xs match {
      case h :: t => {
        val element = h match {
          case Some(bytes) => {
            val column = columns(i)
            column.formatCode match {
              case TextFormat => {
                val strValue = bytes.map(_.toChar).mkString
                Text(column.name, column.columnType, strValue)
              }
              case BinaryFormat => Binary(column.name, column.columnType, bytes)
            }
          }
          case None => Null(columns(i).name, columns(i).columnType)
        }
        loop(t, element :: ys, i + 1)
      }
      case t => ys.reverse
    }
    val elements = loop(x.columnBytes, List.empty[Element], 0)
    new Row(elements)
  })

  def iterator: Iterator[Row] = rows.iterator

  /** The command tag. This is usually a single word that identifies which SQL command was completed.
    *
    * For an INSERT command, the tag is INSERT oid rows, where rows is the number of rows inserted. 
    *  oid is the object ID of the inserted row if rows is 1 and the target table has OIDs;
    *   otherwise oid is 0.
    *
    * For a DELETE command, the tag is DELETE rows where rows is the number of rows deleted.
    * 
    * For an UPDATE command, the tag is UPDATE rows where rows is the number of rows updated.
    *
    * For a SELECT or CREATE TABLE AS command, the tag is SELECT rows where rows is the number of 
    *   rows retrieved.
    *
    * For a MOVE command, the tag is MOVE rows where rows is the number of rows the cursor's 
    *  position has been changed by.
    *
    * For a FETCH command, the tag is FETCH rows where rows is the number of rows that have been 
    *   retrieved from the cursor.
    * @see [[http://www.postgresql.org/docs/current/static/protocol-message-formats.html
    *   CommandComplete]]
    * @note the row count appears only in PostgreSQL 8.2 and later.
    */
  val completedCommand = cc
}

/** Format of data being returned by Postgresql.
  *
  * Currently there are only two types, Text and Binary.
  */
sealed trait FormatCode

/** Data represented by a String format.
  */
case object TextFormat extends FormatCode

/** Data represented in Binary format.
  * @note Postgresql Binary is always BIG ENDIAN.
  */
case object BinaryFormat extends FormatCode


/** A Column of data returned
  * @param name the name of the Column returned from Postgresql
  * @param columnType the data object type id, the "type" of column returned
  * @param formatCode the current format of the data [[roc.postgresql.FormatCode]]
  */
final case class Column private[roc](name: Symbol, columnType: Int, formatCode: FormatCode) {
  final override def toString: String = Column.columnShow.show(this)
}
object Column {
  implicit val columnShow: Show[Column] = new Show[Column] {
    def show(c: Column): String = 
      s"Column(name=${c.name}, columnType=${c.columnType}, formatCode=${c.formatCode})"
  }
}

/** A row returned from a Postgresql Server containing at least one
  *  [[Element]]
  * @param elements a collection of all [[row.postgresql.Element Elements]] returned from 
  *   Postgresql via a query.
  */
final class Row private[postgresql](private[postgresql] val elements: List[Element]) {

  /** Returns the [[roc.postgresql.Element Element]] found via the column name
    *
    * @param columnName the column name given the associated [[roc.postgresql.Element Element]]
    * @return the element found via the column name 
    */
  def get(columnName: Symbol): Element = elements.find(_.name == columnName) match {
    case Some(e) => e
    case None => throw new ElementNotFoundFailure(columnName)
  }

  override final def toString(): String = elements.map(_.toString)
    .foldLeft("")(_ + _)
}

/** Represents and Element in a returned Result
  */
sealed abstract class Element(val name: Symbol, columnType: Int) {

  /** Folds 3 functions to create a value of A
    * @tparam A the type produced by the fold
    * @param fa a function `(String) => A`
    * @param fb a function `Array[Byte] => A`
    * @param fc a function `() => A`
    */
  def fold[A](fa: String => A, fb: Array[Byte] => A, fc: () => A): A = this match {
    case Text(_, _, value)   => fa(value)
    case Binary(_, _, value) => fb(value)
    case Null(_, _)          => fc()
  }

  /** Folds an [[ElementDecoder]] typeclass to create a value of Type `A`
    * @tparam A the Type produced by the Fold
    * @param f an implicit [[ElementDecoder]] typeclass
    * @return A
    */
  def as[A](implicit f: ElementDecoder[A]): A = 
    fold(f.textDecoder, f.binaryDecoder, f.nullDecoder)

  /** Decodes this element as a String
    * @return the String representation of this element
    * @see [[http://www.postgresql.org/docs/current/static/protocol-overview.html
    *   50.1.3 Formats and Format Codes]]
    */
  def asString(): String = fold(
    {(s: String) => s},
    {(bs: Array[Byte]) =>
      throw new UnsupportedDecodingFailure(s"Attempted String decoding of Binary column.")},
    {() => 
      throw new UnsupportedDecodingFailure(s"Attempted String decoding of Null column.")}
  )

  /** Decodes this element as an `Array[Byte]`
    * @return the `Array[Byte]`representation of this element
    * @see [[http://www.postgresql.org/docs/current/static/protocol-overview.html
    *   50.1.3 Formats and Format Codes]]
    * @note Binary representations for integers use network byte order (most significant byte first).
    *  For other data types consult the documentation or source code to learn about the binary 
    *  representation.
    */
  def asBytes(): Array[Byte] = fold(
    {(s: String) => 
      throw new UnsupportedDecodingFailure(s"Attempted Binary decoding of String column.")},
    {(bs: Array[Byte]) => bs},
    {() =>
      throw new UnsupportedDecodingFailure(s"Attempted Binary decoding of Null column.")}
  )
}

case class Null(override val name: Symbol, columnType: Int) extends Element(name, columnType)
case class Text(override val name: Symbol, columnType: Int, value: String) 
  extends Element(name, columnType)
case class Binary(override val name: Symbol, columnType: Int, value: Array[Byte])
  extends Element(name, columnType)

trait ElementDecoder[A] {
  def textDecoder(text: String): A
  def binaryDecoder(bytes: Array[Byte]): A
  def nullDecoder(): A
}
