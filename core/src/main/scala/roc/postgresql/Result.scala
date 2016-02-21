package com.github.finagle
package roc
package postgresql

case class Result(rowDescription: RowDescription, data: List[DataRow])
