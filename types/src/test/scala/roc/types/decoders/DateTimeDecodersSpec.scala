package roc
package types

import io.circe.syntax._
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, 
  ZoneId, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}
import roc.types.failures.{ElementDecodingFailure, NullDecodedFailure}
import roc.types.{decoders => Decoders}
import scala.collection.JavaConverters._

final class DateTimeDecodersSpec extends Specification with ScalaCheck { def is = s2"""

  LocalDate
    must correctly decode Text representation                                      $testValidTextLocalDate
    must throw a ElementDecodingFailure when Text decoding an invalid LocalDate    $testInvalidTextLocalDate
    must correctly decode Binary representation                                    $testValidBinaryLocalDate
    must throw a ElementDecodingFailure when Binary decoding an invalid LocalDate  $testInvalidBinaryLocalDate
    must throw a NullDecodedFailure when Null decoding LocalDate                   $testLocalDateNullDecoding

  LocalTime
    must correctly decode Text representation                                      $testValidLocalTimeText
    must throw a ElementDecodingFailure when Text decoding an invalid LocalTime    $testInvalidLocalTimeText
    must correctly decode Binary representation                                    $testValidLocalTimeBinary
    must throw a ElementDecodingFailure when Binary decoding an invalid LocalTime  $testInvalidLocalTimeBinary
    must throw a NullDecodedFailure when Null decoding LocalDate                   $testLocalTimeNullDecoding

  ZonedDateTime
    must correctly decode Text representation                                         $testValidZonedDateTimeText
    must throw a ElementDecodingFailure when Text decoding an invalid ZonedDateTime   $testInvalidZonedDateTimeText
    must correctly decode Binary representation                                       $testValidZonedDateTimeBinary
    must throw a ElementDecodingFailure when Binary decoding an invalid ZonedDateTime $testInvalidZonedDateTimeBinary
    must throw a NullDecodedFailure when Null decoding LocalDate                      $testZonedDateTimeNullDecoding
                                                                                    """

  private val testValidTextLocalDate = forAll { x: LocalDateStringContainer =>
    Decoders.dateElementDecoders.textDecoder(x.dateString) must_== x.date
  }

  private val testInvalidTextLocalDate = forAll { x: String =>
    Decoders.dateElementDecoders.textDecoder(x) must throwA[ElementDecodingFailure]
  }

  private val testValidBinaryLocalDate = forAll { x: LocalDateBinaryContainer =>
    Decoders.dateElementDecoders.binaryDecoder(x.binaryDate) must_== x.date
  }

  private val testInvalidBinaryLocalDate = forAll { x: Array[Byte] =>
    Decoders.dateElementDecoders.binaryDecoder(x) must throwA[ElementDecodingFailure]
  }

  private val testLocalDateNullDecoding = 
    Decoders.dateElementDecoders.nullDecoder must throwA[NullDecodedFailure]

  private val testValidLocalTimeText = forAll { x: LocalTimeStringContainer =>
    Decoders.localTimeElementDecoders.textDecoder(x.timeString) must_== x.time
  }

  private val testInvalidLocalTimeText = forAll { x: String =>
    Decoders.localTimeElementDecoders.textDecoder(x) must throwA[ElementDecodingFailure]
  }

  private val testValidLocalTimeBinary = forAll { x: LocalTimeBinaryContainer =>
    Decoders.localTimeElementDecoders.binaryDecoder(x.binaryTime) must_== x.time
  }

  private val testInvalidLocalTimeBinary = forAll { x: Array[Byte] =>
    Decoders.localTimeElementDecoders.binaryDecoder(x) must throwA[ElementDecodingFailure]
  }

  private val testLocalTimeNullDecoding =
    Decoders.localTimeElementDecoders.nullDecoder must throwA[NullDecodedFailure]

  private val testValidZonedDateTimeText = forAll { x: ZonedDateTimeStringContainer =>
    Decoders.zonedDateTimeElementDecoders.textDecoder(x.dateTimeString) must_== x.dateTime
  }

  private val testInvalidZonedDateTimeText = forAll { x: String =>
    Decoders.zonedDateTimeElementDecoders.textDecoder(x) must throwA[ElementDecodingFailure]
  }

  private val testValidZonedDateTimeBinary = forAll { x: ZonedDateTimeBinaryContainer =>
    Decoders.zonedDateTimeElementDecoders.binaryDecoder(x.dateTimeBinary) must_== x.dateTime
  }

  private val testInvalidZonedDateTimeBinary = forAll { x: Array[Byte] =>
    Decoders.zonedDateTimeElementDecoders.binaryDecoder(x) must throwA[ElementDecodingFailure]
  }

  private val testZonedDateTimeNullDecoding =
    Decoders.zonedDateTimeElementDecoders.nullDecoder must throwA[NullDecodedFailure]

  case class LocalDateStringContainer(date: LocalDate, dateString: String)
  private implicit lazy val arbitraryLocalDateStringContainer: Arbitrary[LocalDateStringContainer] = 
    Arbitrary(
      for {
        localDate <- arbitrary[LocalDate]
      } yield new LocalDateStringContainer(localDate, localDate.toString)
    )
  case class LocalDateBinaryContainer(date: LocalDate, binaryDate: Array[Byte])
  private implicit lazy val arbitraryLocalDateBinaryContainer: Arbitrary[LocalDateBinaryContainer] = 
    Arbitrary(
      for {
        localDate <- arbitrary[LocalDate]
      } yield new LocalDateBinaryContainer(localDate, localDate.toString.getBytes())
    )

  case class LocalTimeStringContainer(time: LocalTime, timeString: String)
  private implicit lazy val arbitraryLocalTimeStringContainer: Arbitrary[LocalTimeStringContainer] =
    Arbitrary(
      for  {
        localTime <- arbitrary[LocalTime]
        timeString = localTime.toString
      } yield new LocalTimeStringContainer(localTime, timeString)
    )
  case class LocalTimeBinaryContainer(time: LocalTime, binaryTime: Array[Byte])
  private implicit lazy val arbitraryLocalTimeBinaryContainer: Arbitrary[LocalTimeBinaryContainer] =
    Arbitrary(
      for  {
        localTime <- arbitrary[LocalTime]
        binary    = localTime.toString.getBytes()
      } yield new LocalTimeBinaryContainer(localTime, binary)
    )

  private val zonedDateTimeFmt = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd HH:mm:ss")
    .appendFraction(ChronoField.MICRO_OF_SECOND, 0, 6, true)
    .appendOptional(DateTimeFormatter.ofPattern("X"))
    .toFormatter()
  case class ZonedDateTimeStringContainer(dateTime: ZonedDateTime, dateTimeString: String)
  private implicit lazy val arbitraryZonedDateTimeStringContainer: 
    Arbitrary[ZonedDateTimeStringContainer] = Arbitrary(
      for {
        temp           <- arbitrary[ZonedDateTime]
        dateTimeString = temp.format(zonedDateTimeFmt)
        dateTime       = ZonedDateTime.parse(dateTimeString, zonedDateTimeFmt)
      } yield new ZonedDateTimeStringContainer(dateTime, dateTimeString)
    )
  case class ZonedDateTimeBinaryContainer(dateTime: ZonedDateTime, dateTimeBinary: Array[Byte])
  private implicit lazy val arbitraryZonedDateTimeBinaryContainer: 
    Arbitrary[ZonedDateTimeBinaryContainer] = Arbitrary(
      for {
        temp           <- arbitrary[ZonedDateTime]
        dateTimeString = temp.format(zonedDateTimeFmt)
        dateTime       = ZonedDateTime.parse(dateTimeString, zonedDateTimeFmt)
      } yield new ZonedDateTimeBinaryContainer(dateTime, dateTimeString.getBytes())
    )

  private lazy val minInstant: Instant = Instant.EPOCH
  private lazy val maxInstant: Instant = Instant.parse("3000-01-01T00:00:00.00Z")

  private implicit lazy val arbitraryZoneId: Arbitrary[ZoneId] = Arbitrary(
    Gen.oneOf(ZoneId.getAvailableZoneIds.asScala.map(ZoneId.of).toSeq)
  )

  private implicit lazy val arbitraryInstant: Arbitrary[Instant] = Arbitrary(
    Gen.choose(minInstant.getEpochSecond, maxInstant.getEpochSecond).map(Instant.ofEpochSecond)
  )

  private implicit lazy val arbitraryLocalDateTime: Arbitrary[LocalDateTime] = Arbitrary(
    for {
      instant <- arbitrary[Instant]
      zoneId  <- arbitrary[ZoneId]
    } yield LocalDateTime.ofInstant(instant, zoneId)
  )

  private implicit lazy val arbitraryLocalTime: Arbitrary[LocalTime] = Arbitrary(
    for {
      zoneId <- arbitrary[ZoneId]
    } yield LocalTime.now(zoneId)
  )


  private implicit lazy val arbitraryZonedDateTime: Arbitrary[ZonedDateTime] = Arbitrary(
    for {
      instant <- arbitrary[Instant]
      zoneId  <- arbitrary[ZoneId]
    } yield ZonedDateTime.ofInstant(instant, zoneId)
  )

  private implicit lazy val arbitraryOffsetDateTime: Arbitrary[OffsetDateTime] = Arbitrary(
    for {
      instant <- arbitrary[Instant]
      zoneId  <- arbitrary[ZoneId]
    } yield OffsetDateTime.ofInstant(instant, zoneId)
  )

  private implicit lazy val arbitraryLocalDate: Arbitrary[LocalDate] = 
    Arbitrary(arbitrary[LocalDateTime].map(_.toLocalDate))
}
