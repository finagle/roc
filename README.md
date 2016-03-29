![Roc Logo](/roc-logo.png?raw=true "Roc Logo")

roc is a modern [Finagle][finagle] [Postgresql][postgresql] [Client][finagle-client]. What's modern? A Client relying on a [6.x +][finagle-changelog] version of Finagle.

roc is currently under heavy development and is not being published with a current version of `0.0.1-ALPHA`.
If you need a real driver right now you should absolutely use [finagle-postgres][finagle-postgresql-existing].


## Badges
[![PyPI](https://img.shields.io/pypi/l/Django.svg?style=plastic)]()
[![Circle CI](https://circleci.com/gh/penland365/roc/tree/master.svg?style=svg&circle-token=07305c9575ac3fcf0ab5bade8ae2f29921ac04c9)](https://circleci.com/gh/penland365/roc/tree/master)
[![codecov.io](https://codecov.io/github/penland365/roc/coverage.svg?branch=master)](https://codecov.io/github/penland365/roc?branch=master)

## tl;dr
roc is not being published yet. You'll have to clone/fork this repository, then build locally.

Once built, open `sbt console` and insert the following
```scala
scala > :paste
import com.twitter.util.Await
import roc.Postgresql
import roc.postgresql.{Request, Row}

val client = Postgresql.client
  .withUserAndPasswd("username", "password")
  .withDatabase("database")
  .newRichClient("inet!localhost:5432")
val req = new Request("SELECT * FROM STATES;")
val result = Await.result(client.query(req))
```
A result contains two types, a list of columns detailing information about
the result, and a list of rows returned from the database.
```scala
scala> result.columns

res1: List[com.github.finagle.roc.postgresql.Column] =
  List(Column(name='id, columnType=Int4, formatCode=Text),
   Column(name='name, columnType=VarChar, formatCode=Text), Column(name='abbrv, columnType=VarChar, formatCode=Text), Column(name='last_modified_at, columnType=TimestampWithTimezone, formatCode=Text),
   Column(name='inserted_at, columnType=TimestampWithTimezone, formatCode=Text)
  )

scala> :paste

val display = (r: Row) => {
  val id = r.get[Int]('id)
  val name = r.get[String]('name)
  val abbrv = r.get[String]('abbrv)
  val insertedAt = r.get[String]('inserted_at)
  val lastModifiedAt = r.get[String]('last_modified_at)
  println(s"Row $id, $name, $abbrv, $insertedAt, $lastModifiedAt")
}
// Exiting paste mode, now interpreting.

display: com.github.finagle.roc.postgresql.Row => Unit = <function1>

scala> result.rows.map(display(_))
Row 1, North Dakota, ND, 2016-02-18 09:50:55.445246-06, 2016-02-18 09:50:55.445246-06
res4: List[Unit] = List(())
```
At the moment, we can only return `Ints` and `Strings` (I told you it was `ALPHA`).
The good news is that most `Postgresql` columns can be read as strings first, then transformed into the appropriate type
 ( see the handling of Timestamps With TimeZone above).

## Motivation
Why create your own when there is an existing implementation? Two reasons

1. The current [finagle-postgres][finagle-postgresql-existing] was developed pre [Finagle 6.x][finagle-changelog] and does not use modern finagle abstractions. These are core enough to require what amounts to a complete rewrite of the driver.
2. The current project lacks a true owner - Twitter decided to give the Open Source team a [long vacation][twitter-long-vacation] last fall, and since then only a handful of projects are being actively managed.

It is absolutely the goal of this project to become the reference implementation, and hopefully be moved into the [Finagle ecosystem][finagle-ecosystem].


## What's in a name?
roc (*Rokh* or *Rukh*) is named after the Persian mythological bird of prey [Roc][roc-wikipedia],
 which was based in part on the now extinct [elephant bird][elephant-bird-wikipedia]. Yes, that means we've hit the rare 4 point play with the name


 Goal                             |Status
 :-------------------------------:|:----:
 short name                      |✔️   
 obligatory mythological reference|✔️   
 bird reference for Twitter       |✔️   
 elephant reference for Postgresql|✔️   

 High fives for everyone!
 ![Teenage Mutant Ninja Turtles High Fives](http://i.giphy.com/10LNj580n9OmiI.gif)

## Contributors and Participation

roc is currently maintained by [Jeffrey Davis][jeff-davis].

The roc project supports the [Typelevel][typelevel] [code of conduct][code-of-conduct] and wants
all of its channels (GitHub, etc.) to be welcoming environments for everyone.

## License

Licensed under the **[BSD-3-Clause](https://opensource.org/licenses/BSD-3-Clause)**
(Revised BSD Liscense); you may not use this software except in compliance with the License.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

[code-of-conduct]: http://typelevel.org/conduct.html
[elephant-bird-wikipedia]: https://en.wikipedia.org/wiki/Elephant_bird
[finagle]: http://twitter.github.io/finagle/guide/
[finagle-changelog]: http://twitter.github.io/finagle/guide/changelog.html
[finagle-client]: http://twitter.github.io/finagle/guide/Clients.html
[finagle-ecosystem]: https://github.com/finagle
[finagle-postgresql-existing]: https://github.com/finagle/finagle-postgres
[jeff-davis]: https://twitter.com/penland365
[postgresql]: http://www.postgresql.org/
[roc-wikipedia]: https://en.wikipedia.org/wiki/Roc_(mythology)
[twitter-long-vacation]: https://meta.plasm.us/posts/2015/10/13/goodbye-twitter/
[typelevel]: http://typelevel.org/
