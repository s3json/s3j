package s3j.macros.utils

import scala.annotation.targetName
import scala.quoted.{Expr, Quotes}

trait VarHandle[T] {
  /** Variable definition AST node */
  def toDef(using q: Quotes): q.reflect.ValDef

  /** Variable reference */
  def ref: Expr[T]
  
  /** Generate assignment code */
  @targetName("assign")
  def := (assign: Expr[T]): Expr[Unit]
}
