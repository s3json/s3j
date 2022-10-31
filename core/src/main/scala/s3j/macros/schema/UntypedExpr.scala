package s3j.macros.schema

import scala.quoted.{Expr, Type, Quotes}

object UntypedExpr {
  given exprConversion[T]: Conversion[Expr[T], UntypedExpr] = e => new UntypedExpr(e)
}

class UntypedExpr(_inputExpr: Expr[?]) {
  type T
  
  given exprType(using q: Quotes): Type[T] = {
    import q.reflect.*
    _inputExpr.asTerm.tpe.asType.asInstanceOf[Type[T]]
  }
  
  val expr: Expr[T] = _inputExpr.asInstanceOf[Expr[T]]
}
