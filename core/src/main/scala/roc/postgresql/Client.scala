package roc
package postgresql

import com.twitter.finagle._
import com.twitter.finagle.ServiceFactory
import com.twitter.util.{Closable, Future, Time}

object Client {
  def apply (factory: ServiceFactory[Request, Result]): Client = 
    new StdClient(factory)
}

trait Client extends Closable {
  def query(req: Request): Future[Result]
}

final class StdClient(val factory: ServiceFactory[Request, Result]) extends Client {
  private[this] val service = factory.toService

  def query(req: Request): Future[Result] = service(req)

  def close(deadline: Time): Future[Unit] = service.close(deadline)
}
