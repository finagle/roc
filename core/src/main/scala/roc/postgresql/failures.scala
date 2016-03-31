package roc
package postgresql

import cats.data.NonEmptyList
import cats.implicits._
import roc.postgresql.server.PostgresqlMessage

object failures {
  sealed abstract class Failure extends Exception
  
  /** An Error occurring on the Postgresql Server.
    *
    * @see [[roc.postgresql.server.ErrorMessage]]
    */
  final class PostgresqlServerFailure(message: PostgresqlMessage) extends Failure {
    final override def getMessage: String = message.toString
  }
  
  final class UnknownPostgresTypeFailure(objectId: Int) extends Failure {
    final override def getMessage: String = s"Postgres Object ID $objectId is unknown"
  
    def canEqual(a: Any) = a.isInstanceOf[UnknownPostgresTypeFailure]
  
    final override def equals(that: Any): Boolean = that match {
      case x: UnknownPostgresTypeFailure => x.canEqual(this) && getMessage == x.getMessage
      case _ => false
    }
  }
  
  final class ByteDecodingFailure(message: String) extends Failure {
    final override def getMessage: String = message
  }
  
  final class UnsupportedDecodingFailure(message: String) extends Failure {
    final override def getMessage: String = message
  }
  
  final class UnexpectedNoneFailure(message: String) extends Failure {
    final override def getMessage: String = message
  }
  
  /** Denotes, as of Postgresql Protocol 3.0, an unknown Authentication Request Type.
    *
    * @constructor create a new unknown authentication request failure with a request type
    * @param  requestType the integer representing the unkown request type
    * @see [[http://www.postgresql.org/docs/current/static/protocol-message-formats.html 
        Postgresql Message Protocol]]
   */
  final class UnknownAuthenticationRequestFailure(requestType: Int) extends Failure {
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
  final class UnsupportedAuthenticationFailure(messageType: String) extends Failure {
    final override def getMessage: String =
      s"Unsupported Authentication Failure. $messageType authentication is not supported."
  
      def canEqual(a: Any) = a.isInstanceOf[UnsupportedAuthenticationFailure]
  
      final override def equals(that: Any): Boolean = that match {
        case x: UnsupportedAuthenticationFailure => x.canEqual(this) && x.getMessage == getMessage
        case _ => false
      }
  }
  
  final class ColumnNotFoundException(symbol: Symbol) extends Failure {
    final override def getMessage: String = s"Could not find column $symbol in Result"
  }
  
  final class PacketDecodingFailure(message: String) extends Failure {
    final override def getMessage: String = message
  
    def canEqual(a: Any) = a.isInstanceOf[PacketDecodingFailure]
  
    final override def equals(that: Any): Boolean = that match {
      case x: PacketDecodingFailure => x.canEqual(this) && getMessage == x.getMessage
      case _ => false
    }
  }
  
  final class ReadyForQueryDecodingFailure(unknownChar: Char) extends Failure {
    final override def getMessage: String =
      s"Received unexpected Char $unknownChar from Postgres Server."
  
    def canEqual(a: Any) = a.isInstanceOf[ReadyForQueryDecodingFailure]
  
    final override def equals(that: Any): Boolean = that match {
      case x: ReadyForQueryDecodingFailure => x.canEqual(this) && getMessage == x.getMessage
      case _ => false
    }
  }

  /** Denotes a failure to decode a 
    *  [[http://www.postgresql.org/docs/current/static/errcodes-appendix.html PostgresqlMessage]]
    *  from the Postgresql Server
    *
    * @constructor creates an error response decoding failure from all error messages
    * @param xs a [[cats.data.NonEmptyList]] of all decoding failures
    * @note In practice, these should never occur
    */
  final class PostgresqlMessageDecodingFailure private[postgresql]
    (xs: NonEmptyList[String]) extends Failure {
    final override def getMessage(): String = xs.foldLeft("")(_ + _ + " ").trim

    def canEqual(a: Any) = a.isInstanceOf[PostgresqlMessageDecodingFailure]

    final override def equals(that: Any): Boolean = that match {
      case x: PostgresqlMessageDecodingFailure => x.canEqual(this) && x.getMessage == getMessage
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
    extends Failure {
      final override def getMessage: String =
        s"State Transition from $transmittedMessage -> $receivedMessage is undefined."
  }
  
  final class UnknownPostgresqlMessageTypeFailure(char: Char) extends Failure {
    final override def getMessage: String = s"Unknown Postgresql MessageType '$char'."
  }
}
