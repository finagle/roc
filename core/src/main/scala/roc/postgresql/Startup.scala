package roc
package postgresql

import com.twitter.finagle.Stack

/** Represents information required for establishing a connection with a Postgresql server
  */
private[roc] object Startup {

  /** Represents the database name to connect to
    * @constructor create a new Database with a database name
    * @param database the name of a Postgresql Database to connect to
    */ 
  case class Database(db: String)
  implicit object DatabaseParam extends Stack.Param[Database] {
    val default = new Database("postgres")
  }

  /** Represents the Username / Password combination for a Postgresql Connection
    * @constructor create a new Credentials with a username and password
    * @param username the username for a Postgresql connection
    * @param passwd the password for a Postgresql connection
    */
  case class Credentials(username: String, passwd: String)
  implicit object CredentialsParam extends Stack.Param[Credentials] {
    val default = Credentials("postgres", "postgres")
  }

  /** Builds a valid [[roc.postgresql.Startup]] from a [[com.twitter.finagle.Stack]]
    * @param params [[com.twitter.finagle.Stack.Params]]
    */
  def apply(params: Stack.Params): Startup = {
    val Database(db) = params[Database]
    val Credentials(user, passwd) = params[Credentials]
    new Startup(user, passwd, db)
  }
}

/** Contains all information for starting a [[roc.postgresql.ClientDispatcher]]
  * @param username the username for a Postgresql conneciton
  * @param password the password for a Postgresql connection
  * @param database the database for a Postgresql connection
  */
private[roc] case class Startup private[postgresql](username: String, password: String,
  database: String)
