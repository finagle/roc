package roc
package postgresql

import cats.data.Xor
import cats.std.all._
import cats.syntax.eq._
import com.twitter.finagle.dispatch.GenSerialClientDispatcher
import com.twitter.finagle.transport.Transport
import com.twitter.finagle.{Service, WriteException}
import com.twitter.util.{Future, Promise}
import roc.postgresql.transport.{Packet, PacketEncoder}

private[roc] final class ClientDispatcher(trans: Transport[Packet, Packet],
  startup: Startup)
  extends GenSerialClientDispatcher[Request, Result, Packet, Packet](trans) {
  import ClientDispatcher._

  private[this] var backendKeyData: Option[BackendKeyData] = None
  private[this] var mutableParamStatuses: List[ParameterStatus] = Nil

  // this has the potential to be badly constructed if called before
  // the startup phase has been completed
  private[roc] lazy val paramStatuses: Map[String, String] =
    mutableParamStatuses.map(x => (x.parameter, x.value)).toMap

  override def apply(req: Request): Future[Result] = 
    startupPhase.flatMap(_ => super.apply(req))

  /** Performs the Startup phase of a Postgresql Connection.
    *
    * The startup phase is performed once per connection prior to any exchanges
    * between the client and server. Failure to startup renders the service unsuable.
    * The startup phase consists of two separate but sequential  phases 
    * 1. Authentication 2. Server Process setting run time parameters
    * @see [[http://www.postgresql.org/docs/current/static/protocol-flow.html#AEN108589]]
    */
  private[this] val startupPhase: Future[Unit] =
    authenticationPhase.flatMap(_ => serverProcessStartupPhase)

  /** Represents one exchange of FrontendMessage => Future[Message] with the server
    *
    * From the external Client's point of view, Roc maintains the Finagle abstraction of
    * Request => Future[Result]. Internally, there may be many messages passed back and
    * forth between the client and server to build up or complete the Result. This method
    * represents a one-to-one exchange of [[com.github.finagle.roc.postgresql.Messages]]
    * with a Postgresql server.
    */
  private[this] def exchange[A <: FrontendMessage](fm: A)
    (implicit f: PacketEncoder[A]): Future[Message] = trans.write(f(fm)) rescue {
        wrapWriteException
      } before {
        trans.read().flatMap(decode(_))
      }

  /**
   * Returns a Future that represents the result of an exchange
   * between the client and server. An exchange does not necessarily entail
   * a single write and read operation. Thus, the result promise
   * is decoupled from the promise that signals a complete exchange.
   * This leaves room for implementing streaming results.
   */
  override protected def dispatch(req: Request, rep: Promise[Result]): Future[Unit] = {
    val query = new Query(req.query)
    trans.write(encodePacket(query)) rescue {
      wrapWriteException
    } before {
      val signal = new Promise[Unit]
      rep.become(drainTransport(query, signal))
      signal
    }
  }

  private def drainTransport(req: FrontendMessage, signal: Promise[Unit]): Future[Result] = {

    def go(xs: List[RowDescription], ys: List[DataRow]): 
      Future[(List[RowDescription], List[DataRow])] = trans.read()
        .flatMap { packet => decode(packet)
          .flatMap { message => req match {
          case q: Query => message match {
            case rd: RowDescription => go(rd :: xs, ys)
            case dr: DataRow => go(xs, dr :: ys)
            case cc: CommandComplete => go(xs, ys)
            case Idle => signal.setDone(); Future.value((xs, ys))
            case r => println(s"Got $r"); Future.exception(new Exception())
          }
          case _ => Future.exception(new Exception())
          }
        }
      }

      go(List.empty[RowDescription], List.empty[DataRow]).map( tuple => {
        new Result(tuple._1.head, tuple._2)
      })
  }

  private[this] def decode(packet: Packet): Future[Message] = packet.messageType match {
    case Some(mt) if mt === Message.AuthenticationMessageByte =>
      decodePacket[AuthenticationMessage](packet) match {
        case Xor.Right(r) => Future.value(r)
        case Xor.Left(l)  => Future.exception(l)
      }
    case Some(mt) if mt === Message.ErrorByte => decodePacket[ErrorResponse](packet) match {
      case Xor.Right(r) => Future.value(r)
      case Xor.Left(l)  => Future.exception(l)
    }
    case Some(mt) if mt === Message.ParameterStatusByte => 
      decodePacket[ParameterStatus](packet) match {
        case Xor.Right(r) => Future.value(r)
        case Xor.Left(l)  => Future.exception(l)
      }
    case Some(mt) if mt === Message.BackendKeyDataByte => 
      decodePacket[BackendKeyData](packet) match {
        case Xor.Right(r) => Future.value(r)
        case Xor.Left(l)  => Future.exception(l)
      }
    case Some(mt) if mt === Message.ReadyForQueryByte => decodePacket[ReadyForQuery](packet) match {
      case Xor.Right(r) => Future.value(r)
      case Xor.Left(l)  => Future.exception(l)
    }
    case Some(mt) if mt === Message.RowDescriptionByte => 
      decodePacket[RowDescription](packet) match {
        case Xor.Right(r) => Future.value(r)
        case Xor.Left(l)  => Future.exception(l)
      }
    case Some(mt) if mt === Message.DataRowByte => decodePacket[DataRow](packet) match {
      case Xor.Right(r) => Future.value(r)
      case Xor.Left(l)  => Future.exception(l)
    }
    case Some(mt) if mt === Message.CommandCompleteByte => 
      decodePacket[CommandComplete](packet) match {
        case Xor.Right(r) => Future.value(r)
        case Xor.Left(l)  => Future.exception(l)
      }
    case Some(m) => {
        println(m)
        println("INside Some(m)")
        Future.exception(new Exception())
    }
      case None    => {
        close()
        Future.exception(new Exception())
      }
    }

  /** Performs the Authenticaion portion of the Startup Phase
    */
  private[this] def authenticationPhase: Future[Unit] = {
    val sm = StartupMessage(startup.username, startup.database)
    exchange(sm).flatMap(message => message match {
        case AuthenticationOk              => Future.Done
        case AuthenticationClearTxtPasswd  => clearTxtPasswdMachine
        case AuthenticationMD5Passwd(salt) => md5PasswdMachine(salt)
        case AuthenticationKerberosV5      =>
          Future.exception(new UnsupportedAuthenticationFailure("AuthenticationKerberosV5"))
        case AuthenticationSCMCredential   =>
          Future.exception(new UnsupportedAuthenticationFailure("AuthenticationSCMCredential"))
        case AuthenticationSSPI            =>
          Future.exception(new UnsupportedAuthenticationFailure("AuthenticationSSPI"))
        case AuthenticationGSS             =>
          Future.exception(new UnsupportedAuthenticationFailure("AuthenticationGSS"))
        case u => println(u); Future.exception(
          new PostgresqlStateMachineFailure("StartupMessage", u.toString)
        )
    })
  }

  private[this] type ServerProcessValues = (List[ParameterStatus], List[BackendKeyData])
  private[this] def serverProcessStartupPhase: Future[Unit] = {

    def go(safetyCheck: Int, xs: List[ParameterStatus], 
      ys: List[BackendKeyData]): Future[ServerProcessValues] = safetyCheck match {
        // TODO - create an Error type for this
        case x if x > 1000 => Future.exception(new Exception())
        case x if x < 1000 => trans.read().flatMap(packet =>
          decode(packet).flatMap(msg => msg match {
            case p: ParameterStatus  => go(safetyCheck + 1, p :: xs, ys)
            case bkd: BackendKeyData => go(safetyCheck + 1, xs, bkd :: ys)
            case Idle => Future.value((xs, ys))
            case _ => Future.exception(new Exception())
          })
        )
      }

    go(0, List.empty[ParameterStatus], List.empty[BackendKeyData]).flatMap(tuple => {
      mutableParamStatuses = tuple._1
      backendKeyData = tuple._2.headOption
      Future.Done
    })
  }

  /** Performs the AuthenticationCleartextPassword startup sequence
    */
  def clearTxtPasswdMachine: Future[Unit] = {
      val pm = new PasswordMessage(startup.password)
      exchange(pm).flatMap(response => response match {
        case AuthenticationOk => Future.Done
        case ErrorResponse(_, _) => Future.exception(new Exception())
        case u => Future.exception(
          new PostgresqlStateMachineFailure("PasswordMessage", u.toString)
        )
      })
    }


  /** Performs the AuthenticationMD5Password startup sequence
    */
  private[this] def md5PasswdMachine(salt: Array[Byte]): Future[Unit] = {
    val encryptedPasswd = PasswordMessage.encryptMD5Passwd(startup.username, startup.password,
      salt)
    val pm = new PasswordMessage(encryptedPasswd)
    exchange(pm).flatMap(response => response match {
      case AuthenticationOk  => Future.Done
      case er: ErrorResponse => Future.exception(new Exception())
      case u => Future.exception(new PostgresqlStateMachineFailure("PasswordMessage", u.toString))
    })
  }
}
object ClientDispatcher {

  private val wrapWriteException: PartialFunction[Throwable, Future[Nothing]] = {
    case exc: Throwable => Future.exception(WriteException(exc))
  }

  def apply(trans: Transport[Packet, Packet], startup: Startup): Service[Request, Result] =
    new ClientDispatcher(trans, startup)
}
