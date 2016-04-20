package roc
package postgresql

import cats.data.Xor
import cats.std.all._
import cats.syntax.eq._
import com.twitter.finagle.dispatch.GenSerialClientDispatcher
import com.twitter.finagle.transport.Transport
import com.twitter.finagle.{Service, WriteException}
import com.twitter.util.{Future, Promise, Time}
import roc.postgresql.failures.{PostgresqlServerFailure, PostgresqlStateMachineFailure,
  UnsupportedAuthenticationFailure}
import roc.postgresql.server.{ErrorMessage, PostgresqlMessage, WarningMessage}
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
        for {
          packet <- trans.read()
          message <- Message.decode(packet) match {
            case Xor.Left(l)  => Future.exception(l)
            case Xor.Right(m) => Future.value(m)
          }
        } yield message
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
    for {
      _      <- trans.write(encodePacket(query)).rescue(wrapWriteException)
      signal =  rep.become(readTransport(query, new Promise[Unit]))
    } yield signal 
  }

  private[this] def readTransport(req: Transmission, signal: Promise[Unit]): Future[Result] =
    req match {
      case Query(_) => readQueryTx(signal)
    }

  private[this] def readQueryTx(signal: Promise[Unit]): Future[Result] = {

    type Descriptions          = List[RowDescription]
    type Rows                  = List[DataRow]
    type CommandCompleteString = String
    type Collection            = (Descriptions, Rows, CommandCompleteString)
    def go(xs: Descriptions, ys: Rows, ccStr: CommandCompleteString):
      Future[Collection] = trans.read().flatMap(packet => Message.decode(packet) match {
        case Xor.Right(RowDescription(a,b)) => go(RowDescription(a,b) :: xs, ys, ccStr)
        case Xor.Right(DataRow(a,b))        => go(xs, DataRow(a,b) :: ys, ccStr)
        case Xor.Right(EmptyQueryResponse)  => go(xs, ys, "EmptyQueryResponse")
        case Xor.Right(CommandComplete(x))  => go(xs, ys, x)
        case Xor.Right(ErrorResponse(e))    => 
          Future.exception(new PostgresqlServerFailure(e))
        case Xor.Right(NoticeResponse(_))   => go(xs, ys, ccStr) // throw Notice Responses away
        case Xor.Right(Idle)                => Future.value((xs, ys, ccStr))
        case Xor.Right(u) =>
          Future.exception(new PostgresqlStateMachineFailure("Query", u.toString))
        case Xor.Left(l)  => Future.exception(l)
        }
      )

    go(List.empty[RowDescription], List.empty[DataRow], "")
      .map(tuple => {
        val f = signal.setDone()
        new Result(tuple._1, tuple._2, tuple._3)
      })
  }

  /** Closes the connection.
    *
    * We make a best faith effort to inform the Postgresql Server that we are terminating the
    * connection prior to closure.
    * @param deadline the deadline by which the connection must be closed
    */
  override def close(deadline: Time): Future[Unit] =
    trans.write(encodePacket(new Terminate())).ensure {
      super.close(deadline)
      ()
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
        case ErrorResponse(m) => Future.exception(new PostgresqlServerFailure(m))
        case u => println(u); Future.exception(
          new PostgresqlStateMachineFailure("StartupMessage", u.toString)
        )
    })
  }

  private[this] def serverProcessStartupPhase: Future[Unit] = {

    type ParamStatuses = List[ParameterStatus]
    type BKDs = List[BackendKeyData]
    def go(safetyCheck: Int, xs: ParamStatuses, ys: BKDs): Future[(ParamStatuses, BKDs)] = 
      safetyCheck match {
        // TODO - create an Error type for this
        case x if x > 1000 => Future.exception(new Exception())
        case x if x < 1000 => trans.read().flatMap(packet => Message.decode(packet) match {
          case Xor.Left(l) => Future.exception(l)
          case Xor.Right(ParameterStatus(i, j)) => go(safetyCheck + 1, ParameterStatus(i,j) :: xs, ys)
          case Xor.Right(BackendKeyData(i, j)) => go(safetyCheck + 1, xs, BackendKeyData(i, j) :: ys)
          case Xor.Right(Idle) => Future.value((xs, ys))
          case Xor.Right(message) => Future.exception(
            new PostgresqlStateMachineFailure("StartupMessage", message.toString)
          )
        })
      }

    go(0, List.empty[ParameterStatus], List.empty[BackendKeyData]).flatMap(tuple => {
      mutableParamStatuses = tuple._1
      backendKeyData = tuple._2.headOption
      Future.Done
    })
  }

  /** Performs the AuthenticationCleartextPassword startup sequence
    */
  private[this] def clearTxtPasswdMachine: Future[Unit] = {
      val pm = new PasswordMessage(startup.password)
      exchange(pm).flatMap(response => response match {
        case AuthenticationOk => Future.Done
        case ErrorResponse(_) => Future.exception(new Exception())
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
private[roc] object ClientDispatcher {

  private val wrapWriteException: PartialFunction[Throwable, Future[Nothing]] = {
    case exc: Throwable => Future.exception(WriteException(exc))
  }

  def apply(trans: Transport[Packet, Packet], startup: Startup): Service[Request, Result] =
    new ClientDispatcher(trans, startup)
}
