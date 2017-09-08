package roc
package integrations

import com.twitter.finagle.{Addr, Address, Name}
import com.twitter.util.Var
import scala.io.Source

trait Client {
  private val db     = "circle_test"
  //private val db     = "postgres"
  private val user   = "ubuntu"
  //private val user   = "postgres"
  private val passwd = ""
  private val host   = "127.0.0.1"
  private val port   = 5432

  private lazy val address = Address(host, port)
  protected lazy val Postgres = Postgresql.client
    .withUserAndPasswd(user, passwd)
    .withDatabase(db)
    .newRichClient(
      Name.Bound(Var[Addr](Addr.Bound(address)), "roc"),
      "roc"
    )
}

trait SqlReader {

  def readSql(filename: String): String = {
    val path = s"core/src/it/resources/sql/$filename"
    Source.fromFile(path)
      .getLines
      .foldLeft("")(_ + _)
  }
}
