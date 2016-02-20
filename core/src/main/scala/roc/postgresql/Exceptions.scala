package com.github.finagle
package roc
package postgresql

object Exceptions {
  final class InvalidAuthenticationRequest(authType: Int) 
    extends Exception(s"Got invalid and unkown Authentication Request: $authType")
}
