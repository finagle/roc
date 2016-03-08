package roc
package postgresql

import cats.data.Xor

trait PostgresType
object PostgresType {

  def apply(objectId: Int): UnknownPostgresTypeFailure Xor PostgresType = objectId match {
    case Int4OID    => Xor.right(Int4)
    case VarCharOID => Xor.right(VarChar)
    case TimestampWithTimezoneOID => Xor.right(TimestampWithTimezone)
    case n          => Xor.left(new UnknownPostgresTypeFailure(n))
  }

  private[this] val Int4OID = 23
  private[this] val VarCharOID = 1043
  private[this] val TimestampWithTimezoneOID = 1184

  // used for testing only
  private[postgresql] val oids = List(Int4OID, VarCharOID, TimestampWithTimezoneOID)
}

case object Int4 extends PostgresType
case object VarChar extends PostgresType
case object TimestampWithTimezone extends PostgresType
