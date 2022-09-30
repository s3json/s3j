package s3j.macros.traits

import s3j.macros.codegen.Position
import scala.quoted.{Expr, Quotes}

object ErrorReporting {
  private class ErrorReportingImpl(using q: Quotes)(msgTransformer: String => String, defaultPos: q.reflect.Position)
  extends ErrorReporting {
    import q.reflect.{report => rr}

    def error(msg: String): Unit = rr.error(msgTransformer(msg), defaultPos)
    def error(msg: String, pos: Position): Unit = rr.error(msgTransformer(msg), pos.toPos)
    def error(msg: String, context: Expr[Any]): Unit = rr.error(msgTransformer(msg), context)

    def errorAndAbort(msg: String): Nothing = rr.errorAndAbort(msgTransformer(msg))
    def errorAndAbort(msg: String, pos: Position): Nothing = rr.errorAndAbort(msgTransformer(msg), pos.toPos)
    def errorAndAbort(msg: String, context: Expr[Any]): Nothing = rr.errorAndAbort(msgTransformer(msg), context)

    def warning(msg: String): Unit = rr.warning(msgTransformer(msg))
    def warning(msg: String, pos: Position): Unit = rr.warning(msgTransformer(msg), pos.toPos)
    def warning(msg: String, context: Expr[Any]): Unit = rr.warning(msgTransformer(msg), context)

    def info(msg: String): Unit = rr.info(msgTransformer(msg))
    def info(msg: String, pos: Position): Unit = rr.info(msgTransformer(msg), pos.toPos)
    def info(msg: String, context: Expr[Any]): Unit = rr.info(msgTransformer(msg), context)

    def transform(nTransformer: Option[String => String], nPosition: Option[Position]): ErrorReporting =
      new ErrorReportingImpl(
        msgTransformer = nTransformer.fold(msgTransformer)(f => f.andThen(msgTransformer)),
        defaultPos = nPosition.fold(defaultPos)(_.toPos)
      )
  }

  /** Create [[ErrorReporting]] instance from [[Quotes]] instance */
  def fromQuotes(using q: Quotes): ErrorReporting =
    new ErrorReportingImpl(identity, q.reflect.Position.ofMacroExpansion)
}

/**
 * Path-independent version of [[Quotes.reflect.report]] with utility methods to alter default contexts and transform
 * messages
 */
trait ErrorReporting {
  // Reporting methods:

  def error(msg: String): Unit
  def error(msg: String, pos: Position): Unit
  def error(msg: String, context: Expr[Any]): Unit

  def errorAndAbort(msg: String): Nothing
  def errorAndAbort(msg: String, pos: Position): Nothing
  def errorAndAbort(msg: String, context: Expr[Any]): Nothing

  def warning(msg: String): Unit
  def warning(msg: String, pos: Position): Unit
  def warning(msg: String, context: Expr[Any]): Unit

  def info(msg: String): Unit
  def info(msg: String, pos: Position): Unit
  def info(msg: String, context: Expr[Any]): Unit

  /**
   * Derive new [[ErrorReporting]] instance as transformed this instance.
   *
   * @param msgTransformer Transformer to decorate reporting messages, enriching them with additional context
   * @param defaultPos     Position to report if no additional context is specified
   * @return               Derived [[ErrorReporting]] instance
   */
  def transform(
    msgTransformer: Option[String => String] = None,
    defaultPos: Option[Position] = None
  ): ErrorReporting
}
