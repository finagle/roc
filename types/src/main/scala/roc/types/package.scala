package roc

import jawn.ast.JValue
import java.time.{LocalDate, LocalTime, ZonedDateTime}

package object types {
  type Json = JValue
  type Date = LocalDate
  type Time = LocalTime
  type TimestampWithTZ = ZonedDateTime
}
