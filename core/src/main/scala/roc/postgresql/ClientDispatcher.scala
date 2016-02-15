package com.github.finagle
package roc
package postgresql

import com.google.common.cache.{CacheBuilder, RemovalListener, RemovalNotification}
import com.twitter.finagle.dispatch.GenSerialClientDispatcher
import com.twitter.finagle.transport.Transport
import com.twitter.finagle.{CancelledRequestException, Service, WriteException, ServiceProxy}
import com.twitter.util.{Future, Promise, Return, Try, Throw}

import com.github.finagle.roc.postgresql.transport.Packet
import cats.syntax.eq._
import cats.std.all._

final class ClientDispatcher(trans: Transport[Packet, Packet],
  startup: Startup)
  extends GenSerialClientDispatcher[Message, Message, Packet, Packet](trans) {
  import ClientDispatcher._

  private[this] var backendKey: Option[BackendKeyData] = None
  private[this] var ps: List[ParameterStatusMessage] = Nil

  override def apply(req: Message): Future[Message] = {
    startupPhase flatMap { _ =>
      super.apply(req)
    }
  }

  val startupPhase: Future[Message] = {
    val sm = StartupMessage(startup.username, startup.database)
    super.apply(sm)
  }

  /**
   * Returns a Future that represents the result of an exchange
   * between the client and server. An exchange does not necessarily entail
   * a single write and read operation. Thus, the result promise
   * is decoupled from the promise that signals a complete exchange.
   * This leaves room for implementing streaming results.
   */
  override protected def dispatch(req: Message, rep: Promise[Message]): Future[Unit] = {
    println(req)
    trans.write(req.toPacket) rescue {
      wrapWriteException
    } before {
      val signal = new Promise[Unit]
      rep.become(readTransport(rep))
      signal.setDone()
      signal
    }
  }

  private[this] def readTransport(rep: Promise[Message]): Future[Message] = {
    def go(rep: Promise[Message]): Future[Message] = trans.read() flatMap { packet => 
      decodePacket(packet) flatMap { message => 
        message match {
          case s: StartupMessages => println(s"Startup Message $s"); go(rep)
          case p: ParameterStatusMessage => {
            ps :+ p
            go(rep)
          }
          case bkd: BackendKeyData => {
            backendKey = Some(bkd)
            go(rep)
          }
          case rd: RowDescription => go(rep)
          case r => {
            println(s"Got unknown $r")
            Future.value(message)
          }
        }
      }
    }

    go(rep)
  }

  private[this] def decodePacket(packet: Packet): Future[Message] = packet.messageType match {
    case Some(mt) if mt.toByte === Message.AuthenticationRequest => StartupMessages(packet)
    case Some(mt) if mt.toByte === Message.ErrorMessage => ErrorMessage(packet)
    case Some(mt) if mt === Message.ParameterStatus =>  ParameterStatusMessage(packet)
    case Some(mt) if mt === Message.BackendKeyData => BackendKeyData(packet)
    case Some(mt) if mt === Message.ReadyForQuery =>  ReadyForQuery(packet)
    case Some(mt) if mt === Message.RowDescription => RowDescription(packet)
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
  private val wrapWriteException: PartialFunction[Throwable, Future[Nothing]] = {
    case exc: Throwable => Future.exception(WriteException(exc))
  }

  def apply(trans: Transport[Packet, Packet], startup: Startup): Service[Message, Message] = 
    new ClientDispatcher(trans, startup)

}
