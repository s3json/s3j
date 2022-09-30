import s3j.macros.codegen.{CodeUtils, NameGenerator, Variable}

import scala.annotation.experimental
import scala.quoted.{Expr, Quotes, Type, quotes}

@experimental
object TestMacro {
  inline def showCode[T](inline f: T): Unit = ${ showCodeImpl('{ f }) }
  inline def placeholder[T]: T = ${ CodeUtils.placeholderValue[T] }

  private def showCodeImpl[T](f: Expr[T])(using q: Quotes): Nothing = {
    import q.reflect.*
    report.info("AST:\n" + f.asTerm.show(using Printer.TreeStructure))
    report.errorAndAbort("Code:\n" + f.asTerm.show(using Printer.TreeShortCode))
  }
}
