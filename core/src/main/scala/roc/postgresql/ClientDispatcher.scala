package com.github.finagle
package roc
package postgresql

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.google.common.cache.{CacheBuilder, RemovalListener, RemovalNotification}
import com.twitter.finagle.dispatch.GenSerialClientDispatcher
import com.twitter.finagle.transport.Transport
import com.twitter.finagle.{CancelledRequestException, Service, WriteException, ServiceProxy}
import com.twitter.util.{Future, NoFuture, Promise, Return, Try, Throw}

import java.util.concurrent.atomic.AtomicReference

import com.github.finagle.roc.postgresql.transport.Packet
import cats.syntax.eq._
import cats.std.all._

import cats.data.Xor

final class ClientDispatcher(trans: Transport[Packet, Packet],
  startup: Startup)
  extends GenSerialClientDispatcher[FrontendMessage, Message, Packet, Packet](trans) {
  import ClientDispatcher._

  private[this] val state = new AtomicReference[Future[_]](Off)

  private[this] var backendKey: Option[BackendKeyData] = None
  private[this] var ps: List[ParameterStatusMessage] = Nil

  override def apply(req: FrontendMessage): Future[Message] = state.get match {
    case Off => {
      state.set(On)
      startupPhase.flatMap { _ => super.apply(req) }
    }
    case On => super.apply(req)
  }

  val startupPhase: Future[Message] = {
    val sm = StartupMessage(startup.username, startup.database)
    super.apply(sm)
      .flatMap(message => message match {
        case AuthenticationOk => Future.value(message)
        case AuthenticationClearTxtPasswd => {
          super.apply(new PasswordMessage(startup.password))
            .flatMap(response => response match {
              case AuthenticationOk => Future.value(response)
              case ReadyForQuery('I') => Future.value(response)
              case _ => Future.exception(new Exception())
            })
        }
        case AuthenticationMD5Passwd(salt) => {
          val encryptedPasswd = PasswordMessage.encryptMD5Passwd(startup.username, 
            startup.password, salt)
          super.apply(new PasswordMessage(encryptedPasswd))
            .flatMap(response => response match {
              case AuthenticationOk => Future.value(response)
              case ReadyForQuery('I') => Future.value(response)
              case _ => Future.exception(new Exception())
            })
        }
        case ReadyForQuery('I') => Future.value(message)
        case r => println(r); Future.exception(new Exception())
      })
  }

  /**
   * Returns a Future that represents the result of an exchange
   * between the client and server. An exchange does not necessarily entail
   * a single write and read operation. Thus, the result promise
   * is decoupled from the promise that signals a complete exchange.
   * This leaves room for implementing streaming results.
   */
  override protected def dispatch(req: FrontendMessage, rep: Promise[Message]): Future[Unit] = {
    trans.write(req.encode) rescue {
      wrapWriteException
    } before {
      val signal = new Promise[Unit]
      rep.become(drainTransport(req, signal))
      signal
    }
  }

  private def drainTransport(req: FrontendMessage, signal: Promise[Unit]): Future[Message] = {
    def go(signal: Promise[Unit]): Future[Message] = trans.read() flatMap { packet => 
      decodePacket(packet) flatMap { message => 
        req match {
          case s: StartupMessage => message match {
            case AuthenticationClearTxtPasswd => signal.setDone(); Future.value(message)
            case AuthenticationMD5Passwd(_) => signal.setDone(); Future.value(message)
            case AuthenticationOk => go(signal)
            case p: ParameterStatusMessage => ps :+ p; go(signal)
            case bkd: BackendKeyData => backendKey = Some(bkd); go(signal)
            case ReadyForQuery('I') => signal.setDone(); Future.value(message)
            case r => println(r); Future.exception(new Exception())
          }
          case pm: PasswordMessage => message match {
            case AuthenticationOk => go(signal)
            case p: ParameterStatusMessage => ps :+ p; go(signal)
            case bkd: BackendKeyData => backendKey = Some(bkd); go(signal)
            case ReadyForQuery('I') => signal.setDone(); Future.value(message)
            case r => println(r); Future.exception(new Exception())
          }
          case q: Query => message match {
            case rd: RowDescription => println(rd); go(signal)
            case dr: DataRow => println(dr); go(signal)
            case cc: CommandComplete => println(cc); go(signal)
            case ReadyForQuery('I') => println("all done"); signal.setDone(); Future.value(message)
            case _ => Future.exception(new Exception())
          }
          case _ => Future.exception(new Exception())
          }
        }
      }

    go(signal)
  }

  private[this] def decodePacket(packet: Packet): Future[Message] = packet.messageType match {
    case Some(mt) if mt === Message.AuthenticationRequest => AuthenticationMessage(packet)
    case Some(mt) if mt === Message.ErrorMessage => ErrorMessage(packet)
    case Some(mt) if mt === Message.ParameterStatus =>  ParameterStatusMessage(packet)
    case Some(mt) if mt === Message.BackendKeyData => BackendKeyData(packet)
    case Some(mt) if mt === Message.ReadyForQuery =>  ReadyForQuery(packet)
    case Some(mt) if mt === Message.RowDescription => RowDescription(packet)
    case Some(mt) if mt === Message.DataRow => DataRow(packet)
    case Some(mt) if mt === Message.CommandComplete => CommandComplete(packet)
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
}
object ClientDispatcher {

  private sealed trait ConnStates extends NoFuture
  private case object Off extends ConnStates
  private case object On extends ConnStates

  private val wrapWriteException: PartialFunction[Throwable, Future[Nothing]] = {
    case exc: Throwable => Future.exception(WriteException(exc))
  }

  def apply(trans: Transport[Packet, Packet], startup: Startup): Service[FrontendMessage, Message] =
    new ClientDispatcher(trans, startup)
}
