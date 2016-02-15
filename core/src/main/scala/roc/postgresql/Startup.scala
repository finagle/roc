package com.github.finagle
package roc
package postgresql

import com.twitter.finagle.Stack

object Startup {

  case class Credentials private(username: String, database: String)
  object Credentials {
    def apply(username: String, someDb: Option[String]): Credentials = someDb match {
      case Some(db) => new Credentials(username, db)
      case None     => new Credentials(username, username)
    }
  }

  implicit object CredentialsParam extends Stack.Param[Credentials] {
    val default = Credentials("postgres", None)
  }

  case class Password(password: String)
  implicit object PasswordParam extends Stack.Param[Password] {
    val default = new Password("")
  }

  def apply(params: Stack.Params): Startup = {
   val Credentials(u, d) = params[Credentials]
   val Password(p)       = params[Password]
   new Startup(u, p, d) 
  }
}

case class Startup private(username: String, password: String, database: String)
