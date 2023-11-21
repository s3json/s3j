package s3j.internal

import s3j.format.JsonFormat
import s3j.internal.cclass.CaseClassGenerator
import s3j.internal.enums.EnumGenerator
import s3j.internal.utils.GenerationMode

import scala.quoted.{Expr, Quotes, Type}
import scala.util.control.NonFatal

object InternalMacros {
  inline def derived[T]: T = ${ derivedImpl[T] }

  inline def derivedDebug[T]: T = ${ derivedDebugImpl[T] }

  private def derivedImpl[T](using q: Quotes, t: Type[T]): Expr[T] = {
    import q.reflect.*

    // Decompose generated type into generation mode and actual type
    type R
    val (mode, innerTypeRepr) = GenerationMode.decode(TypeRepr.of[T])
    given innerType: Type[R] = innerTypeRepr.asType.asInstanceOf[Type[R]]

    try {
      if (CaseClassGenerator.isCaseClass[R]) new CaseClassGenerator[R](mode).result.asExprOf[T]
      else if (EnumGenerator.isEnum[R]) new EnumGenerator[R](mode).result.asExprOf[T]
      else report.errorAndAbort("Unsupported type: " + Type.show[R])
    } catch {
      case e =>
        e.printStackTrace()
        throw e
    }
  }

  private def derivedDebugImpl[T](using q: Quotes, t: Type[T]): Expr[T] = {
    import q.reflect.*
    report.errorAndAbort(derivedImpl[T].show)
  }
}
