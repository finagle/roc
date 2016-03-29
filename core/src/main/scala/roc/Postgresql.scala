package roc

import com.twitter.finagle.client.{DefaultPool, StackClient, StdStackClient, Transporter}
import com.twitter.finagle.netty3.Netty3Transporter
import com.twitter.finagle.transport.Transport
import com.twitter.finagle.{Name, Service, ServiceFactory, Stack}
import com.twitter.util.{Duration, Future}
import roc.postgresql.transport.{Packet, PostgresqlClientPipelineFactory}
import roc.postgresql.{Request, Result, Startup}

/** Rich builder methods for [[roc.Postgresql]]
  *
  * Supplements a Finagle Client with convenient builder methods for
  * constructing a Postgresql Client.
  */
trait PostgresqlRichClient { self: com.twitter.finagle.Client[Request, Result] =>

  /** 
    * Creates a new `RichClient` connected to a logical destination described
    * by `dest` with the assigned `label`. The `label` is used to scope Client stats.
    */
  def newRichClient(dest: Name, label: String): postgresql.Client =
    postgresql.Client(newClient(dest, label))

  /**
    * Creates a new `RichClient` connected to the logical destination described
    * by `dest`.
    */
  def newRichClient(dest: String): postgresql.Client =
    postgresql.Client(newClient(dest))
}

/** A Finagle Client that connects to Postgresql Server(s).
  *
  * @example {{{
  * val client = Postgresql.client
  *   .withDatabase("postgres")
  *   .withUserAndPasswd("username", "password")
  *   .newRichClient("localhost:5432")
  * }}}
  */
object Postgresql extends com.twitter.finagle.Client[Request, Result] 
  with PostgresqlRichClient {

  /**
    * Implements a Postgresql client in terms of a Finagle Stack Client.
    * The client inherits a wealth of features from Finagle including connection 
    * pooling and load balancing.
    */
  case class Client(
    stack: Stack[ServiceFactory[Request, Result]] = StackClient.newStack,
    params: Stack.Params = StackClient.defaultParams + DefaultPool.Param(
        low = 0, high = 1, bufferSize = 0,
        idleTime = Duration.Top,
        maxWaiters = Int.MaxValue)
  ) extends StdStackClient[Request, Result, Client] with PostgresqlRichClient {
    protected def copy1(
      stack: Stack[ServiceFactory[Request, Result]] = this.stack,
      params: Stack.Params = this.params
    ): Client = copy(stack, params)

  protected type In = Packet
  protected type Out = Packet
  protected def newTransporter = Netty3Transporter[Packet, Packet](
    PostgresqlClientPipelineFactory, StackClient.defaultParams)
  protected def newDispatcher(transport: Transport[Packet, Packet]): 
    Service[Request, Result] = postgresql.ClientDispatcher(transport, Startup(params))
  override def configured[P](psp: (P, Stack.Param[P])): Client = super.configured(psp)

  /** The database name for this Postgresql Client to connect to.
    * @param database the name of the database to connect to
    */
  def withDatabase(database: String): Client =
    configured(Startup.Database(database))

  /** The username and password for this Postgresql Client to connect with.
    * @param username the username for the Postgresql conneciton
    * @param passwd the password for the Postgresql conneciton
    */
  def withUserAndPasswd(username: String, passwd: String): Client = 
    configured(Startup.Credentials(username, passwd))
  }

  /** Used to construct a Client via the builder pattern.
    */
  val client = Client()

  def newClient(dest: Name, label: String): ServiceFactory[Request, Result] = 
    client.newClient(dest, label)

  def newService(dest: Name, label: String): Service[Request, Result] =
    client.newService(dest, label)
}
