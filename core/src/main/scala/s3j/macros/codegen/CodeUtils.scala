package s3j.macros.codegen

import scala.quoted.*

object CodeUtils {
  def placeholderValue[T](using Quotes, Type[T]): Expr[T] =
    (Type.of[T] match {
      case '[ AnyRef ]  => '{ null }
      case '[ Boolean ]   => Expr(false)
      case '[ Byte ]      => Expr(0.toByte)
      case '[ Short ]     => Expr(0.toShort)
      case '[ Int ]       => Expr(0)
      case '[ Long ]      => Expr(0L)
      case '[ Char ]      => Expr('\u0000')
      case '[ Float ]     => Expr(0.0F)
      case '[ Double ]    => Expr(0.0D)
      case '[ Unit ]      => '{ () }
      case _ => quotes.reflect.report.errorAndAbort("Unsupported type for placeholder: " + Type.show[T])
    }).asInstanceOf[Expr[T]]

  /** Place all statements into a single block, using last block as a result */
  def joinBlocks[T](blocks: Seq[Expr[T]])(using Quotes, Type[T]): Expr[T] = {
    if (blocks.isEmpty) {
      throw new IllegalAccessException("joinBlocks with empty blocks (at least one is needed for result)")
    }

    joinBlocks(blocks.init, blocks.last)
  }

  /** Place all statements into a single block, removing nested blocks for readability of generated code */
  def joinBlocks[T](blocks: Seq[Expr[Any]], result: Expr[T])(using q: Quotes, t: Type[T]): Expr[T] = {
    import q.reflect.*
    val ret = List.newBuilder[Statement]

    def processStatement(s: Statement): Unit =
      s match {
        case Block(code, result) =>
          for (s <- code) processStatement(s)
          processStatement(result)

        case _ => ret += s
      }

    for (b <- blocks) processStatement(b.asTerm)

    val finalResult = result.asTerm match {
      case Block(code, r) =>
        for (s <- code) processStatement(s)
        r

      case other => other
    }

    Block(ret.result(), finalResult).asExprOf[T]
  }
}
