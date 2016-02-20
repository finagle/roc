package com.github.finagle
package roc

import com.twitter.finagle._
import com.twitter.finagle.client.{DefaultPool, StackClient, Transporter, StdStackClient}
import com.twitter.finagle.netty3.{Netty3Transporter, Netty3Listener}
import com.twitter.finagle.server.{StackServer, Listener, StdStackServer}
import com.twitter.finagle.service.{RetryPolicy, TimeoutFilter}
import com.twitter.finagle.transport.Transport
import com.twitter.finagle.util.DefaultTimer
import com.twitter.util.{Await, Duration, Future}
import java.net.SocketAddress

import com.github.finagle.roc.postgresql.transport.{Packet, PostgresqlClientPipelineFactory}
import com.github.finagle.roc.postgresql.{FrontendMessage, Message}
import com.github.finagle.roc.postgresql.Startup

trait PostgresqlRichClient { self: com.twitter.finagle.Client[FrontendMessage, Message] => 

  def newRichClient(dest: Name, label: String): postgresql.Client = 
    postgresql.Client(newClient(dest, label))

  def newRichClient(dest: String): postgresql.Client = 
    postgresql.Client(newClient(dest))
}

object Postgresql extends com.twitter.finagle.Client[FrontendMessage, Message] 
  with PostgresqlRichClient {

  case class Client(
    stack: Stack[ServiceFactory[FrontendMessage, Message]] = StackClient.newStack,
    params: Stack.Params = StackClient.defaultParams + DefaultPool.Param(
        low = 0, high = 1, bufferSize = 0,
        idleTime = Duration.Top,
        maxWaiters = Int.MaxValue)
  ) extends StdStackClient[FrontendMessage, Message, Client] with PostgresqlRichClient {
    protected def copy1(
      stack: Stack[ServiceFactory[FrontendMessage, Message]] = this.stack,
      params: Stack.Params = this.params
    ): Client = copy(stack, params)

  protected type In = Packet
  protected type Out = Packet
  protected def newTransporter = Netty3Transporter[Packet, Packet](
    PostgresqlClientPipelineFactory, StackClient.defaultParams)
  protected def newDispatcher(transport: Transport[Packet, Packet]): 
    Service[FrontendMessage, Message] = postgresql.ClientDispatcher(transport, Startup(params))
  override def configured[P](psp: (P, Stack.Param[P])): Client = super.configured(psp)

  def withCredentials(username: String, database: Option[String]): Client = 
    configured(Startup.Credentials(username, database))

  def withPassword(password: String): Client = 
    configured(Startup.Password(password))
  }

  val client = Client()

  def newClient(dest: Name, label: String): ServiceFactory[FrontendMessage, Message] = 
    client.newClient(dest, label)

  def newService(dest: Name, label: String): Service[FrontendMessage, Message] =
    client.newService(dest, label)
}
