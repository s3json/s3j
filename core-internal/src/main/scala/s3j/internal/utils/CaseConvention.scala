package s3j.internal.utils

object CaseConvention {
  def splitWords(s: String): Seq[String] =
    s.replaceAll("([a-z])([A-Z])", "$1_$2").toLowerCase.split("_")

  def transform(cc: CaseConvention, str: String): String = cc match {
    case CaseConvention.NoConvention  => str
    case CaseConvention.KebabCase     => splitWords(str).mkString("-")
  }
}

enum CaseConvention {
  case NoConvention
  case KebabCase
}
