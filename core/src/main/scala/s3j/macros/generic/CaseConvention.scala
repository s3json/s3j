package s3j.macros.generic

import scala.collection.mutable

object CaseConvention {
  /** Split a string into words, each word is returned in lowercase */
  def parseWords(str: String): Seq[String] = {
    val rb = Vector.newBuilder[String]
    val sb = new mutable.StringBuilder

    var i = 0
    while (i < str.length) {
      str.charAt(i) match {
        case '_' | '-' =>
          if (sb.nonEmpty) {
            rb += sb.result()
            sb.clear()
          }

        case ch if Character.isUpperCase(ch) =>
          if (sb.nonEmpty && !Character.isUpperCase(str.charAt(i - 1))) {
            rb += sb.result()
            sb.clear()
          }

          sb += Character.toLowerCase(ch)

        case ch => sb += ch
      }

      i += 1
    }

    if (sb.nonEmpty) {
      rb += sb.result()
    }

    rb.result()
  }

  /** No transformations are applied */
  case object NoConvention extends CaseConvention {
    def transform(str: String): String = str
    def concat(a: String, b: String): String = a + b
  }

  /** Case convention for `camelCase` */
  case object CamelCase extends CaseConvention {
    def transform(str: String): String =
      parseWords(str).zipWithIndex.map {
        case (s, 0) => s // first word is unchanged
        case (s, _) => s.capitalize
      }.mkString

    def concat(a: String, b: String): String = a + b.capitalize
  }

  /** Case convention for `PascalCase` */
  case object PascalCase extends CaseConvention {
    def transform(str: String): String = parseWords(str).map(_.capitalize).mkString
    def concat(a: String, b: String): String = a + b.capitalize
  }

  /** Case convention for `snake_case` */
  case object SnakeCase extends CaseConvention {
    def transform(str: String): String = parseWords(str).mkString("_")
    def concat(a: String, b: String): String = a + "_" + b
  }

  /** Case convention for `SCREAMING_SNAKE_CASE` */
  case object ScreamingSnakeCase extends CaseConvention {
    def transform(str: String): String = parseWords(str).map(_.toUpperCase).mkString("_")
    def concat(a: String, b: String): String = a + "_" + b
  }

  /** Case convention for `kebab-case` */
  case object KebabCase extends CaseConvention {
    def transform(str: String): String = parseWords(str).mkString("-")
    def concat(a: String, b: String): String = a + "-" + b
  }

  /** Case convention for `Capitalized-Kebab-Case` */
  case object CapitalizedKebabCase extends CaseConvention {
    def transform(str: String): String = parseWords(str).map(_.capitalize).mkString("-")
    def concat(a: String, b: String): String = a + "-" + b
  }
}

trait CaseConvention {
  /** @return String transformed into appropriate case convention */
  def transform(str: String): String

  /** @return Two strings concatenated according to case convention */
  def concat(a: String, b: String): String
}
