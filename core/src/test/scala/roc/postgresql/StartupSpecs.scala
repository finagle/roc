package roc
package postgresql

import com.twitter.finagle.client.StackClient
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2._
import roc.postgresql.Startup.{Database, Credentials}

final class StartupSpecs extends Specification with ScalaCheck { def is = s2"""

  Database
    must have correct database name $testDatabase

  Credentials
    must have correct username and password $testUserAndPasswd

  Startup
    must have correct database, username, and password              $testStartupClass
    must have correct defaults for username, password, and database $testStartupDefaults
                                                                            """

  val testDatabase = forAll { dbContainer: DbContainer =>
    val database = dbContainer.db
    database.db must_== dbContainer.dbName
  }

  val testUserAndPasswd = forAll { credentialsContainer: CredentialsContainer =>
    val expectedCredentials = Credentials(credentialsContainer.username, 
      credentialsContainer.passwd) 
    credentialsContainer.credentials must_== expectedCredentials
  }

  val testStartupClass = forAll { startupContainer: StartupContainer =>
    val expectedStartup = Startup(startupContainer.username, startupContainer.passwd,
      startupContainer.database)
    startupContainer.startup must_== expectedStartup
  }

  val testStartupDefaults= {
    val expectedStartup = Startup("postgres", "postgres", "postgres")
    Startup(StackClient.defaultParams) must_== expectedStartup
  }

  case class DbContainer(db: Database, dbName: String)
  private lazy val databaseGen: Gen[DbContainer] = for {
    db  <-  arbitrary[String]
  } yield DbContainer(Database(db), db)
  implicit lazy val arbitraryDatabase: Arbitrary[DbContainer] =
    Arbitrary(databaseGen)

  case class CredentialsContainer(credentials: Credentials, username: String, passwd: String)
  private lazy val credentialsContainerGen: Gen[CredentialsContainer] = for {
    username    <-  arbitrary[String]
    password    <-  arbitrary[String]
  } yield CredentialsContainer(Credentials(username, password), username, password)
  implicit lazy val arbitraryCredentialsContainer: Arbitrary[CredentialsContainer] =
    Arbitrary(credentialsContainerGen)

  case class StartupContainer(startup: Startup, username: String, passwd: String, database: String)
  private lazy val startupContainerGen: Gen[StartupContainer] = for {
    username    <-  arbitrary[String]
    passwd      <-  arbitrary[String]
    database    <-  arbitrary[String]
  } yield StartupContainer(Startup(username, passwd, database), username, passwd, database)
  implicit lazy val arbitraryStartupContainer: Arbitrary[StartupContainer] =
    Arbitrary(startupContainerGen)
}
