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
  private[this] var ps: List[ParameterStatus] = Nil

  override def apply(req: Request): Future[Result] =
    state.get match {
      case Off => for {
        _     <-  authenticate
        _     <-  completeStartup
        resp  <-  super.apply(req)
      } yield resp
      case On => super.apply(req)
    }

  def exchange[A <: FrontendMessage](fm: A)(implicit f: PacketEncoder[A]): Future[Message] = 
    trans.write(f(fm)) rescue {
      wrapWriteException
    } before {
      trans.read().flatMap(decode(_))
    }

  def authenticate: Future[Unit] = {
    val sm = StartupMessage(startup.username, startup.database)
    exchange(sm)
      .flatMap(message => message match {
        case AuthenticationOk => Future.Done 
        case AuthenticationClearTxtPasswd => {
          exchange(new PasswordMessage(startup.password))
            .flatMap(response => response match {
              case AuthenticationOk => Future.Done 
              case Idle => Future.Done
              case _ => Future.exception(new Exception())
            })
        }
        case AuthenticationMD5Passwd(salt) => {
          val encryptedPasswd = PasswordMessage.encryptMD5Passwd(startup.username, 
            startup.password, salt)
          exchange(new PasswordMessage(encryptedPasswd))
            .flatMap(response => response match {
              case AuthenticationOk => Future.Done 
              case Idle => Future.Done
              case _ => Future.exception(new Exception())
            })
        }
        case Idle => Future.Done
        case r => println(r); Future.exception(new Exception())
      })
  }

  def completeStartup: Future[Unit] = {

    def go(xs: List[ParameterStatus], ys: List[BackendKeyData]):
      Future[(List[ParameterStatus], List[BackendKeyData])] = trans.read()
      .flatMap{ packet => decode(packet).flatMap{ msg => msg match {
        case p: ParameterStatus  => go(p :: xs, ys)
        case bkd: BackendKeyData => go(xs, bkd :: ys)
        case Idle => Future.value((xs, ys))
        case _ => Future.exception(new Exception())
      }}
    }

    go(List.empty[ParameterStatus], List.empty[BackendKeyData])
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
    case Some(mt) if mt === Message.ErrorByte => decodePacket[ErrorMessage](packet) match {
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
