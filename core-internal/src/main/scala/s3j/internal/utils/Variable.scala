package s3j.internal.utils

import s3j.internal.utils.Utils

import scala.annotation.targetName
import scala.quoted.*

object Variable {
  /** Type conversion to directly include variables in splices */
  given variableToExpr[T]: Conversion[Variable[T], Expr[T]] = _.value

  private class VariableImpl[T](name: String, initCode: Quotes ?=> Expr[T], val const: Boolean)
                               (using q: Quotes, t: Type[T])
  extends Variable[T] {
    import q.reflect.*
    private val _symbol: Symbol = Symbol.newVal(Symbol.spliceOwner, name, TypeRepr.of[T],
      if (const) Flags.EmptyFlags else Flags.Mutable, Symbol.noSymbol)

    val init: Expr[T] = initCode(using _symbol.asQuotes)
    def symbol(using Quotes): quotes.reflect.Symbol =
      _symbol.asInstanceOf[quotes.reflect.Symbol]

    def definition(using Quotes): quotes.reflect.ValDef = {
      import quotes.reflect.*
      ValDef(symbol, Some(init.asTerm))
    }

    val value: Expr[T] = Ref(_symbol).asExprOf[T]

    @targetName("assign")
    def := (x: Expr[T]): Expr[Unit] = Assign(Ref(_symbol), x.asTerm).asExprOf[Unit]
  }

  /**
   * Create new variable handle initialized with appropriate placeholder value (`null` for objects,
   * `0` for numbers, etc)
   *
   * @param name  Name prefix for debugging purposes
   * @tparam T    Type of variable
   * @return      Variable handle
   */
  def create[T](name: String)(using Quotes, Type[T]): Variable[T] =
    new VariableImpl[T](name, Utils.placeholderValue[T], false)

  /**
   * Create new variable handle initialized with user-provided value.
   *
   * @param name  Name prefix for debugging purposes
   * @param init  Value for variable initialization
   * @tparam T    Type of variable
   * @return      Variable handle
   */
  def create[T](name: String)(init: Quotes ?=> Expr[T])(using Quotes, Type[T]): Variable[T] =
    new VariableImpl[T](name, init, false)

  /**
   * Prepend a code block with variable definitions. Variable definitions are not Term's, so they cannot be included
   * directly in splices. Variables are defined in sequence order, and their initialization blocks are executed from
   * first to last.
   */
  def defineVariables[T](vars: Seq[Variable[_]], code: Expr[T])(using Quotes, Type[T]): Expr[T] = {
    import quotes.reflect.*
    val (statements: Seq[Statement], result: Term) =
      code.asTerm match {
        case Block(st, res) => st -> res
        case other => Nil -> other
      }

    Block((vars.map(_.definition) ++ statements).toList, result).asExprOf[T]
  }
}

/**
 * `Expr[T]`-like class representing a variable handle. Allows to generate multiple unrelated [[Expr]]s pointing to
 * same variable, which is impossible in vanilla API.
 */
trait Variable[T] {
  /** @return Whether this variable is a constant ('val') */
  def const: Boolean

  /** @return Variable definition */
  def definition(using Quotes): quotes.reflect.ValDef

  /**
   * Read stored value.
   *
   * @return Expression with variable reference
   */
  def value: Expr[T]

  /**
   * Update stored value.
   *
   * @param x Value to assign
   * @return  Code with assignment
   */
  @targetName("assign")
  def := (x: Expr[T]): Expr[Unit]
}
