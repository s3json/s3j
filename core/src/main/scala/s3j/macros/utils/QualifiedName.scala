package s3j.macros.utils

import scala.quoted.*

object QualifiedName {
  /** Capture `QualifiedName[T]` object for a given type */
  transparent inline given derived[T]: QualifiedName[T] = ${ captureTypeNameImpl[T] }

  private def captureTypeNameImpl[T](using q: Quotes, t: Type[T]): Expr[QualifiedName[T]] = {
    import q.reflect.*
    '{ QualifiedName[T]( ${Expr(TypeRepr.of[T].typeSymbol.fullName)} ) }
  }
}

/** Type class for capturing fully qualified name of the type */
case class QualifiedName[T](name: String)
