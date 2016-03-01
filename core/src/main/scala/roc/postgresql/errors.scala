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

/** Denotes an Unsupported Authentication Request
  * @constructor create a new unsupported authentication request failure with a request type
  * @param messageType the Authentication Message that is unsupported
  * @see [[http://www.postgresql.org/docs/current/static/protocol-message-formats.html 
      Postgresql Message Protocol]]
 */
final class UnsupportedAuthenticationFailure(messageType: String) extends Error {
  final override def getMessage: String =
    s"Unsupported Authentication Failure. $messageType authentication is not supported."

    def canEqual(a: Any) = a.isInstanceOf[UnsupportedAuthenticationFailure]

    final override def equals(that: Any): Boolean = that match {
      case x: UnsupportedAuthenticationFailure => x.canEqual(this) && x.getMessage == getMessage
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


/** Denotes a State Transition with the Postgresql State Machine that should be impossible.
  *
  * The Postgresql 3.0 Protocol describes serveral specific connection State Machines depending
  * on the phase of the connection - StartupPhase ( made up of Authentication Phase and
  * Server Process Startup Phase ), SimpleQuery Phase. During these phases, it is possible
  * for a Postgresql Server to transmit a Message that does not make sense given the current
  * State of the Connection. In practice, these should be extremely rare.
  * @constructor create a new postgresql state machine failure with a messsage type
  * @param transmittedMessage the Message transmitted to the Postgresql Server
  * @param recievedMessage the Message recieved from the Postgresql Server that caused the state 
  *     transition failure
  * @see [[http://www.postgresql.org/docs/current/static/protocol-flow.html]]
 */
final class PostgresqlStateMachineFailure(transmittedMessage: String, receivedMessage: String)
  extends Error {
    final override def getMessage: String =
      s"State Transition from $transmittedMessage -> $receivedMessage is undefined."
}
