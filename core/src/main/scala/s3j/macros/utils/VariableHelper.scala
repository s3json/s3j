package s3j.macros.utils

import s3j.macros.codegen.CodeUtils
import s3j.macros.traits.BasicPluginContext

import scala.annotation.{targetName, threadUnsafe}
import scala.quoted.{Expr, Quotes, Type}

class VariableHelper[Q <: Quotes](using val q: Q)(owner: q.reflect.Symbol, context: BasicPluginContext) {
  import q.reflect.*

  private class VarHandleImpl[T](val baseName: String, val tpe: Type[T], val init: Term, val isVariable: Boolean)
  extends VarHandle[T] {
    @threadUnsafe
    lazy val name: String = context.freshName(baseName)

    @threadUnsafe
    lazy val symbol: Symbol = Symbol.newVal(
      parent = owner,
      name = name,
      tpe = TypeRepr.of(using tpe),
      flags = if (isVariable) Flags.Mutable else Flags.EmptyFlags,
      privateWithin = Symbol.noSymbol
    )

    def toDef(using q: Quotes): q.reflect.ValDef =
      q.reflect.ValDef(symbol.asInstanceOf[q.reflect.Symbol], Some(init.asInstanceOf[q.reflect.Term]))

    @targetName("assign")
    def :=(assign: Expr[T]): Expr[Unit] =
      Assign(Ref(symbol), assign.asTerm).asExprOf[Unit]

    def ref: Expr[T] =
      Ref(symbol).asExprOf[T](using tpe)
  }

  /** Create new variable initialized with a placeholder */
  def makeVar[T](name: String)(using t: Type[T]): VarHandle[T] =
    makeVar(name)(CodeUtils.placeholderValue[T])

  def makeVar[T](name: String)(init: Expr[T])(using t: Type[T]): VarHandle[T] =
    new VarHandleImpl[T](name, t, init.asTerm, isVariable = true)
}
