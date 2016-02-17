package com.github.finagle
package roc
package postgresql

import com.twitter.finagle.ServiceFactory
import com.twitter.finagle._
import com.twitter.util.{Closable, Future, Time}


object Client {
  def apply (factory: ServiceFactory[FrontendMessage, Message]): Client = 
    new StdClient(factory)
}

trait Client extends Closable {
  def query(m: Query): Future[Message]
}

final class StdClient(val factory: ServiceFactory[FrontendMessage, Message]) extends Client {
  private[this] val service = factory.toService

  def query(m: Query): Future[Message] =
    service(m)

  def close(deadline: Time): Future[Unit] = service.close(deadline)
}
