package s3j.macros

import scala.quoted.*

object JsonMacros {
  inline def derived[T]: T = ${ derivedMacro[T] }

  private def derivedMacro[T](using q: Quotes, tt: Type[T]): Expr[T] = {
    import q.reflect.*
    throw new RuntimeException(GenerationMode.decode(TypeRepr.of[T]).toString)
    ???
  }
}
