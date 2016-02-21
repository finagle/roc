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

import scala.collection.mutable.ListBuffer

import java.util.concurrent.atomic.AtomicReference

import com.github.finagle.roc.postgresql.transport.Packet
import cats.syntax.eq._
import cats.std.all._

import cats.data.Xor

final class ClientDispatcher(trans: Transport[Packet, Packet],
  startup: Startup)
  extends GenSerialClientDispatcher[Request, Result, Packet, Packet](trans) {
  import ClientDispatcher._

  private[this] val state = new AtomicReference[Future[_]](Off)

  private[this] var backendKeyData: Option[BackendKeyData] = None
  private[this] var ps: List[ParameterStatusMessage] = Nil

  override def apply(req: Request): Future[Result] =
    state.get match {
      case Off => for {
        _     <-  authenticate
        _     <-  completeStartup
        resp  <-  super.apply(req)
      } yield resp
      case On => super.apply(req)
    }

  def exchange(fm: FrontendMessage): Future[Message] = trans.write(fm.encode)
    .rescue(wrapWriteException)
    .before {
      trans.read().flatMap(decodePacket(_))
    }

  def authenticate: Future[Unit] = {
    val sm = StartupMessage(startup.username, startup.database)
    exchange(sm)
      .flatMap(message => message match {
        case AuthenticationOk => Future.Done //Future.value(message)
        case AuthenticationClearTxtPasswd => {
          exchange(new PasswordMessage(startup.password))
            .flatMap(response => response match {
              case AuthenticationOk => Future.Done // Future.value(response)
              case ReadyForQuery('I') => Future.Done // Future.value(response)
              case _ => Future.exception(new Exception())
            })
        }
        case AuthenticationMD5Passwd(salt) => {
          val encryptedPasswd = PasswordMessage.encryptMD5Passwd(startup.username, 
            startup.password, salt)
          exchange(new PasswordMessage(encryptedPasswd))
            .flatMap(response => response match {
              case AuthenticationOk => Future.Done // Future.value(response)
              case ReadyForQuery('I') => Future.Done // Future.value(response)
              case _ => Future.exception(new Exception())
            })
        }
        case ReadyForQuery('I') => Future.Done // Future.value(message)
        case r => println(r); Future.exception(new Exception())
      })
  }

  def completeStartup: Future[Unit] = {

    def go(xs: List[ParameterStatusMessage], ys: List[BackendKeyData]):
      Future[(List[ParameterStatusMessage], List[BackendKeyData])] = trans.read()
      .flatMap{ packet => decodePacket(packet).flatMap{ msg => msg match {
        case p: ParameterStatusMessage => go(p :: xs, ys)
        case bkd: BackendKeyData       => go(xs, bkd :: ys)
        case ReadyForQuery('I')        => Future.value((xs, ys))
        case _ => Future.exception(new Exception())
      }}
    }

    go(List.empty[ParameterStatusMessage], List.empty[BackendKeyData])
      .flatMap(tuple => {
        ps = tuple._1
        backendKeyData = tuple._2.headOption
        state.set(On)
        Future.Done
      })
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
    trans.write(query.encode) rescue {
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
        .flatMap { packet => decodePacket(packet)
          .flatMap { message => req match {
          case q: Query => message match {
            case rd: RowDescription => go(rd :: xs, ys)
            case dr: DataRow => go(xs, dr :: ys)
            case cc: CommandComplete => go(xs, ys)
            case ReadyForQuery('I') => signal.setDone(); Future.value((xs, ys))
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

  def apply(trans: Transport[Packet, Packet], startup: Startup): Service[Request, Result] =
    new ClientDispatcher(trans, startup)
}
