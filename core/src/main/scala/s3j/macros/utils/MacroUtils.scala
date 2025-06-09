package s3j.macros.utils

import s3j.internal.macros.ForbiddenMacroUtils

import java.io.{PrintWriter, StringWriter}
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}

private[macros] object MacroUtils {
  val UsePluginPrefix = "s3j:usePlugin="
  val ModifierPrefix  = "s3j:@"

  // Example settings:
  //
  //  -Xmacro-settings:s3j:usePlugin=com.something.MeowPlugin
  //  -Xmacro-settings:s3j:@s3j.annotations.base64url
  //  -Xmacro-settings:s3j:@s3j.annotations.naming.defaultFieldCase=ScreamingSnakeCase

  /** @return All strings starting with given prefix, with that prefix removed */
  private def stripPrefixes(xs: Seq[String], prefix: String): Seq[String] =
    xs.collect { case s if s.startsWith(prefix) => s.substring(prefix.length) }

  /**
   * XmacroSettings is @experimental, and I don't want that to pollute my code. This method will become obsolete once
   * experimental-ness is removed.
   *
   * @param q Quotes instance
   * @return List of macro settings filtered by `s3j:` prefix
   */
  def macroSettings(using q: Quotes): Seq[String] =
    ForbiddenMacroUtils.instance.macroSettings

  /** @return Macro settings, filtered by given prefix (and with that prefix removed) */
  def macroSettings(prefix: String)(using Quotes): Seq[String] =
    stripPrefixes(macroSettings, prefix)
}
