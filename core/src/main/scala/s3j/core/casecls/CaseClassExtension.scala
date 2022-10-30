package s3j.core.casecls

import s3j.core.casecls.CaseClassContext.{FieldRequest, GenerationOutcome, GenerationUnsupported}
import s3j.macros.generic.Extensions

import scala.quoted.{Quotes, Type}

case object CaseClassExtension extends Extensions.Key[CaseClassExtension]("caseClass")

abstract class CaseClassExtension {
  def processField[T](using CaseClassContext)(field: FieldRequest[T])(using Quotes, Type[T]): GenerationOutcome[T] = 
    GenerationUnsupported
}
