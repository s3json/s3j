package s3j.macros.utils

import java.io.{PrintWriter, StringWriter}
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}

object MacroUtils {
  val SettingsPrefix = "s3j:"

  /** @return All strings starting with given prefix, with that prefix removed */
  def stripPrefixes(xs: Seq[String], prefix: String): Seq[String] =
    xs.collect { case s if s.startsWith(prefix) => s.substring(prefix.length) }

  /**
   * XmacroSettings is @experimental, and I don't want that to pollute my code. This method will become obsolete once
   * experimental-ness is removed.
   *
   * @param q Quotes instance
   * @return List of macro settings filtered by `s3j:` prefix
   */
  def macroSettings(using q: Quotes): Seq[String] = {
    import q.reflect.*
    val method = CompilationInfo.getClass.getMethod("XmacroSettings")
    stripPrefixes(method.invoke(CompilationInfo).asInstanceOf[List[String]].toVector, SettingsPrefix)
  }
}
