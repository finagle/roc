![Roc Logo](/roc-logo.png?raw=true "Roc Logo")

roc is a modern [Finagle][finagle] [Postgresql][postgresql] [Client][finagle-client]. What's modern? A Client relying on a [6.x +][finagle-changelog] version of Finagle.

## Badges
[![PyPI](https://img.shields.io/pypi/l/Django.svg?style=plastic)]()
[![Maven Central](https://img.shields.io/maven-central/v/com.github.finagle/roc-core_2.11.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/com.github.finagle/roc-core_2.11)
[![Codecov branch](https://img.shields.io/codecov/c/github/finagle/roc/master.svg?style=plastic)](https://codecov.io/github/finagle/roc?branch=master)
[![CircleCI branch](https://img.shields.io/circleci/project/finagle/roc/master.svg?style=plastic)](https://circleci.com/gh/finagle/roc/tree/master)
[![Gitter](https://img.shields.io/badge/gitter-join%20chat-green.svg?style=plastic)](https://gitter.im/finagle/roc?)


## tl;dr
Roc is published to [Maven Central], so for the latest stable version add the following to your build:
```scala
libraryDependencies ++= Seq(
  "com.github.finagle" %% "roc-core"  % "0.0.3",
  "com.github.finagle" %% "roc-types" % "0.0.3"
)
```
Roc is under heavy development, so to stay up to with the latest `SNAPSHOT` version add the following to your build instead:
```scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "com.github.finagle"  %% "roc-core" % "0.0.4-SNAPSHOT" changing()
)
```

Open `sbt console` and insert the following
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
result: roc.postgresql.Result = Result(Text('id,23,1)Text('name,25,Alabama)Text('abbrv,1043,AL), Text('id,23,2)...)
```
Let's turn a `Result` into all 50 `State(s)`.
```scala
import java.time.ZonedDateTime
import roc.types.decoders._

case class State(id: Int, name: String, abbrv: String, insertedAt: ZonedDateTime)
val row2State: (Row) => (State) = (row: Row) => {
  val id = row.get('id).as[Int]
  val name = row.get('name).as[String]
  val abbrv = row.get('abbrv).as[String]
  val insertedAt = row.get('inserted_at).as[TimestampWithTZ]
  State(id, name, abbrv, insertedAt)
}
val states = result.map(row2State).toList
states: List[State] = List(State(1,Alabama,AL,2016-05-10T11:59:13.879709-05:00), State(2,Alaska,AK,2016-05-10T11:59:20.974995-05:00))
```
If you're into Scaladocs ( I am ), they can be found [here][Scaladocs].

## Tell me about Result
The most important type in Roc is [Result](http://finagle.github.io/roc/docs/#roc.postgresql.Result), the type returned after a Postgresql query is executed. Result implements [Iterable](http://www.scala-lang.org/api/current/#scala.collection.Iterable) so that it can be viewed as a collection of [Rows](http://finagle.github.io/roc/docs/#roc.postgresql.Row).
The two additional members of `Result` are:
```scala
val result = client.query(new Request("SELECT * FROM FOO;"))

result.columns // all column information returned from the Request
result.completedCommand // a String representation of what happend
```
#### Result.completedCommand
* For an INSERT command, the tag is INSERT oid rows, where rows is the number of rows inserted. oid is the object ID of the inserted row if rows is 1 and the target table has OIDs; otherwise oid is 0.
* For a DELETE command, the tag is DELETE rows where rows is the number of rows deleted.
* For an UPDATE command, the tag is UPDATE rows where rows is the number of rows updated.
* For a SELECT or CREATE TABLE AS command, the tag is SELECT rows where rows is the number of rows retrieved.
* For a MOVE command, the tag is MOVE rows where rows is the number of rows the cursor's position has been changed by.
* For a FETCH command, the tag is FETCH rows where rows is the number of rows that have been retrieved from the cursor.

For an `UPDATE` or `INSERT` command, a `Result` will have a length of `0` and no column information, but will always return a `completedCommand`.
From Postgresql's perspective, the fact that the query returns without giving an error is evidence that the command completed successfully, and `Roc` will adhere to their style, not the `JDBC` style.

### Row
A `Row` holds a non-zero number of [Elements](http://finagle.github.io/roc/docs/#roc.postgresql.Element).
An `Element` is the actual value returned at `[row][column]`.
For example, if we were to execute the following:
```scala
scala> val req = new Request("SELECT COUNT(*) FROM STATES;")
scala> val result = Await.result(client.query(req))
result: roc.postgresql.Result = Result(Text('count,20,50))
```
we are given a `Result` with one `Column` (with the name of `'count`, a FormatCode of Text, and OID of 20), and one `Row`.
To retrieve a value out of that `Row`, we do the following:
```scala
scala> val head = result.head // let's just get the first row
scala> val count = head.get('count)
count: roc.postgresql.Element = Text('count,20,50)
```
Wait, what exactly is an `Element`?

### Elements
`roc-core` has an extremely minimal design philosophy. That includes the decoding of actual data. In other words,
we'll decode the bytes into the correct format, we'll tell you what that format is, but it's up to you (or another roc module)
to go any further.
Postgreql returns data in 3 possible formats:
1. UTF-8 Text
2. Binary Format (typically Big Endian)
3. No data is returned for that column ( mean it is a NULL value )

`roc-core` will decode this data into the given format, but goes no further in the process - it is up to core clients to decide how to procede.
Going back to the example above, we see:
```scala
scala> val count = head.get('count)
count: roc.postgresql.Element = Text('count,20,50)
```
This means that Postgresql has returned a column name `'count`, in a String format, with a Postgresql Type of `Long`.
String encodings are typically preferred, and almost universally the case unless the column returned is binary data,
or if a `FETCH` command is used to return a `CURSOR`.
An `Element` has 3 sub-types

1. Text
2. Binary
3. NULL

Yes, we've deliberately introduced a specific `NULL` type into the system. This allows clients to handle
`NULL` cases in whatever way they see fit.

To get a value out of an `Element`, you have several options:
```scala
scala> val count = head.get('count)
count: roc.postgresql.Element = Text('count,20,50)

scala> count.asString
res4: String = 50

scala> count.asBytes
roc.postgresql.failures$UnsupportedDecodingFailure: Attempted Binary decoding of String column.
```

If you attempt to get the String of a Binary element, you'll get another `UnsupportedDecodingFailure`.
The two attempts above are short cuts to getting values. The preferred method involves a fold:
```scala
def fold[A](fa: String => A, fb: Array[Byte] => A, fc: () => A): A = this match {
  case Text(_, _, value)   => fa(value)
  case Binary(_, _, value) => fb(value)
  case Null(_, _)          => fc()
}
```
 This allows you to handle `NULL` values in any way you see fit, and makes the decode process typesafe.
 Both `asString` and `asBytes` call `fold` under the covers.

 Finally, there is the ubiquitous parsing / decoding method `as[A]`:
 ```scala
 def as[A](implicit f: ElementDecoder[A]): A = fold(f.textDecoder, f.binaryDecoder, f.nullDecoder)
 ```
 An `ElementDecoder` is a TypeClass to allow custom decoding in a more syntax friendly way. See the [Scaladocs]
 or gitter for more information.

## decoders
The `roc-types` project defines type aliases from `Postgresql => Scala`, and includes `ElementDecoder` instances for those types. The current types include

* `smallint => Short`
* `int     => Int`
* `bigint => Long`
* `real => Float`
* `double precision => Double`
* `char => Char (Note this is a C-Style understanding of a Char, not a UTF Rune)`
* `text/CHARACTER VARYING => String`
* `bool    => Boolean`
* `JSON/JSONB => Json` (via Jawn)
* `Date => Date = java.time.LocalDate`
* `Time => Time = java.time.LocalTime`
*  `TIME WITH TIME ZONE => TimestampWithTZ = java.time.ZonedDateTime`
* `NULL => Option`

### Optional Decoders
To decode a column that may be NULL, clients should simply use an `Option[A]` decoder, where `A = COLUMN TYPE`.
Let's add a `population` column to our `states` table, of type `int`.
```scala
case class State(id: Int, name: String, abbrv: String, population: Option[Int],
insertedAt: ZonedDateTime)
val row2State: (Row) => (State) = (row: Row) => {
  val id = row.get('id).as[Int]
  val name = row.get('name).as[String]
  val abbrv = row.get('abbrv).as[String]
  val population = row.get('population).as[Int]
  val insertedAt = row.get('inserted_at).as[TimestampWithTZ]
  State(id, name, abbrv, population, insertedAt)
}
val state = result.map(row2State).toList.head
states: State = State(1,Alabama,AL,None,2016-05-10T12:46:59.998788-05:00)
```
As the type of column should be known at compile time, `roc-types` throws an `NullDecodedFailure(TYPE)`
with a helpful error message if you attempt to decode a `NULL` type:
```scala
scala> val row = result.head
row: roc.postgresql.Row = Text('inserted_at,1184,2016-05-1012:46:59.998788-05)Null('population,23)Text('abbrv,1043,AL)Text('name,25,Alabama)Text('id,23,1)
scala> val population = row.get('population).as[Int]
roc.types.failures$NullDecodedFailure: A NULL value was decoded for type INT. Hint: use the Option[INT] decoder, or ensure that Postgres cannot return NULL for the requested value.
```

## Design Philosophy
The desire of `core` is to be as minimal as possible. In practice, that means mapping a Finagle Service over Postgresql with as little bedazzling as possible.
The Postgresql Client will be very minimalistic ( currently just one method, `def query(Request): Future[Result]`), and the aim is for `Result` to do as little as possible.
Additional future modules may provide additional functionality.

## Motivation
The current [finagle-postgres][finagle-postgresql-existing] was developed pre [Finagle 6.x][finagle-changelog] and does not use modern finagle abstractions. These are core enough to require what amounts to a complete rewrite of the driver.

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
all of its channels (GitHub, gitter, etc.) to be welcoming environments for everyone.

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
[Maven Central]: http://search.maven.org/
[postgresql]: http://www.postgresql.org/
[roc-wikipedia]: https://en.wikipedia.org/wiki/Roc_(mythology)
[scaladocs]:  http://finagle.github.io/roc/docs/
[typelevel]: http://typelevel.org/
