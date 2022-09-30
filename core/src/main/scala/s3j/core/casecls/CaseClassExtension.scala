package s3j.core.casecls

import s3j.core.casecls.CaseClassContext.{FieldRequest, GenerationOutcome, GenerationUnsupported}
import s3j.macros.generic.Extensions

import scala.quoted.{Quotes, Type}

object CaseClassExtension {
  val key: Extensions.Key[CaseClassExtension] = Extensions.key("caseClass")
}

abstract class CaseClassExtension {
  def processField[T](using CaseClassContext)(field: FieldRequest[T])(using Quotes, Type[T]): GenerationOutcome[T] = 
    GenerationUnsupported
}
