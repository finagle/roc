package roc
package types

import roc.postgresql.failures.Failure

object failures {

  final class BinaryDecodingUnsupportedFailure(unsupportedType: String) extends Failure {
    final override def getMessage: String = 
      s"Binary decoding of type $unsupportedType is currently unsupported."
  }

  final class TextDecodingUnsupportedFailure(unsupportedType: String) extends Failure {
    final override def getMessage: String = 
      s"Text decoding of type $unsupportedType is currently unsupported."
  }

  final class NullDecodedFailure(unsupportedType: String) extends Failure {
    final override def getMessage: String = 
      s"A NULL value was decoded for type $unsupportedType. Hint: use the Option[$unsupportedType] decoder, or ensure that Postgres cannot return NULL for the requested value."
  }

  final class ElementDecodingFailure(elementDescription: String, throwable: Throwable)
    extends Failure {
    final override def getMessage: String = 
      s"Failure to decode $elementDescription. ${throwable.getMessage()}"
  }

}
