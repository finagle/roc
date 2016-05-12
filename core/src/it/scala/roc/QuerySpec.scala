package roc
package integrations

import com.twitter.finagle.{Addr, Address, Name, Service}
import com.twitter.util.Var
import com.twitter.util.Await
import org.specs2.Specification
import roc.postgresql.Request
import scala.io.Source

final class QuerySpec extends Specification 
  with SqlReader
  with Client { def is = sequential ^ s2""" 

  Query
    must execute a CREATE command   $testCreate
    must execute an INSERT command  $testInsert
    must execute an UPDATE command  $testUpdate
    must execute a SELECT command   $testSelect
    must execute a DELETE command   $testDelete
    must execute a DROP command     $testDrop

                                                                        """

  def testCreate() = {
    val sql     = readSql("query/create.sql")
    val request = new Request(sql)
    val result  = Await.result(Postgres.query(request))
    result.completedCommand must_== "CREATE TABLE"
  }

  def testInsert() = {
    val sql     = readSql("query/insert.sql")
    val request = new Request(sql)
    val result  = Await.result(Postgres.query(request))
    result.completedCommand must_== "INSERT 0 1"
  }

  def testUpdate() = {
    val sql     = readSql("query/update.sql")
    val request = new Request(sql)
    val result  = Await.result(Postgres.query(request))
    result.completedCommand must_== "UPDATE 1"
  }

  def testSelect() = {
    val request = new Request("SELECT name FROM roc_tests WHERE id = 1")
    val result  = Await.result(Postgres.query(request))
    result.toList.length must_== 1
  }

  def testDelete() = {
    val request = new Request("DELETE FROM roc_tests WHERE id = 1;")
    val result  = Await.result(Postgres.query(request))
    result.completedCommand must_== "DELETE 1"
  }

  def testDrop() = {
    val request = new Request("DROP TABLE roc_tests;")
    val result = Await.result(Postgres.query(request))
    result.completedCommand must_== "DROP TABLE"
  }
}
