package com.github.finagle
package roc
package postgresql

sealed trait Error extends Exception

final class UnknownPostgresTypeFailure(objectId: Int) extends Error {
  final override def getMessage: String = s"Postgres Object ID $objectId is unknown"

  def canEqual(a: Any) = a.isInstanceOf[UnknownPostgresTypeFailure]

  final override def equals(that: Any): Boolean = that match {
    case x: UnknownPostgresTypeFailure => x.canEqual(this) && getMessage == x.getMessage
    case _ => false
  }
}

final class ByteDecodingFailure(message: String) extends Error {
  final override def getMessage: String = message
}

final class UnsupportedDecodingFailure(message: String) extends Error {
  final override def getMessage: String = message
}

final class UnexpectedNoneFailure(message: String) extends Error {
  final override def getMessage: String = message
}

/** Denotes, as of Postgresql Protocol 3.0, an unknown Authentication Request Type.
  *
  * @constructor create a new unknown authentication request failure with a request type
  * @param  requestType the integer representing the unkown request type
  * @see [[http://www.postgresql.org/docs/current/static/protocol-message-formats.html 
      Postgresql Message Protocol]]
 */
final class UnknownAuthenticationRequestFailure(requestType: Int) extends Error {
  final override def getMessage: String =
    s"Unknown Authentication Request Type: $requestType"

  def canEqual(a: Any) = a.isInstanceOf[UnknownAuthenticationRequestFailure]

  final override def equals(that: Any): Boolean = that match {
    case x: UnknownAuthenticationRequestFailure => x.canEqual(this) && x.getMessage == getMessage
    case _ => false
  }
}

final class ColumnNotFoundException(symbol: Symbol) extends Error {
  final override def getMessage: String = s"Could not find column $symbol in Result"
}

final class PacketDecodingFailure(message: String) extends Error {
  final override def getMessage: String = message

  def canEqual(a: Any) = a.isInstanceOf[PacketDecodingFailure]

  final override def equals(that: Any): Boolean = that match {
    case x: PacketDecodingFailure => x.canEqual(this) && getMessage == x.getMessage
    case _ => false
  }
}

final class ReadyForQueryDecodingFailure(unknownChar: Char) extends Error {
  final override def getMessage: String =
    s"Received unexpected Char $unknownChar from Postgres Server."

  def canEqual(a: Any) = a.isInstanceOf[ReadyForQueryDecodingFailure]

  final override def equals(that: Any): Boolean = that match {
    case x: ReadyForQueryDecodingFailure => x.canEqual(this) && getMessage == x.getMessage
    case _ => false
  }
}
