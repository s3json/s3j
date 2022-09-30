package s3j.macros.utils

import scala.collection.mutable
import scala.quoted.Quotes

object ReportingUtils {
  /** Version of `reportError` taking optional position */
  def reportError(using q: Quotes)(message: String, pos: Option[q.reflect.Position] = None): Nothing =
    pos match {
      case Some(p) => q.reflect.report.errorAndAbort(message, p)
      case None => q.reflect.report.errorAndAbort(message)
    }

  /** Format stack trace, dropping all entries not related to s3j invocation */
  def formatException(cause: Throwable): String = {
    val sb = new mutable.StringBuilder
    val dejaVu: mutable.Set[IdentityWrapper[Throwable]] = mutable.HashSet.empty

    def countMacroFrames(a: Array[StackTraceElement]): Int = {
      var x = a.length - 1
      while (x >= 0 && !a(x).getClassName.startsWith("s3j.")) x -= 1
      a.length - 1 - x
    }

    def countCommonFrames(a: Array[StackTraceElement], b: Array[StackTraceElement]): Int = {
      var x = a.length - 1
      var y = b.length - 1

      while (x >= 0 && y >= 0 && a(x) == b(y)) {
        x -= 1
        y -= 1
      }

      a.length - 1 - x
    }

    def formatNestedException(root: Boolean, e: Throwable, prefix: String, caption: String,
                              enclosing: Array[StackTraceElement]): Unit = {
      val identityWrapper = IdentityWrapper(e)
      sb ++= prefix ++= caption

      if (dejaVu.contains(identityWrapper)) {
        sb ++= "[circular reference: " ++= e.toString ++= "]\n"
        return
      }

      dejaVu.add(identityWrapper)

      val trace = e.getStackTrace
      val commonFrames =
        if (root) countMacroFrames(trace)
        else countCommonFrames(trace, enclosing)

      sb ++= e.toString ++= "\n"
      for (i <- 0 until (trace.length - commonFrames)) {
        sb ++= prefix ++= caption ++= "\tat " ++= trace(i).toString ++= "\n"
      }

      if (!root && commonFrames > 0) {
        sb ++= prefix ++= "\t... "
        sb.append(commonFrames)
        sb ++= "more\n"
      }

      for (s <- e.getSuppressed) {
        formatNestedException(root = false, s, prefix + "\t", "Suppressed: ", trace)
      }

      if (e.getCause != null) {
        formatNestedException(root = false, e.getCause, prefix, "Caused by: ", trace)
      }
    }

    formatNestedException(root = true, cause, "", "", Array.empty)
    sb.result()
  }

  /** Format exception with stack trace */
  def formatException(message: String, cause: Throwable): String = {
    val sb = new mutable.StringBuilder()
    sb ++= message
    if (!message.endsWith("\n")) sb += '\n'
    sb += '\n' ++= formatException(cause)
    sb.result()
  }

  /** Report macro expansion exception preserving BOTH stack trace and position of failed element */
  def reportException(using q: Quotes)(message: String, cause: Throwable,
                                       pos: Option[q.reflect.Position] = None): Nothing =
    reportError(formatException(message, cause), pos)
}
