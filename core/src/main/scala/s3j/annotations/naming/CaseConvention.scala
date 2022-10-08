package s3j.annotations.naming

import s3j.annotations.naming.CaseConvention.{CamelCase, KebabCase, PascalCase}

import scala.collection.mutable

enum CaseConvention {
  /** Original names are preserved */
  case NoConvention

  /** camelCase */
  case CamelCase

  /** snake_case */
  case SnakeCase

  /** SCREAMING_SNAKE_CASE */
  case ScreamingSnakeCase

  /** kebab-case */
  case KebabCase

  /** Capitalized-Kebab-Case */
  case CapitalizedKebabCase

  /** PascalCase */
  case PascalCase

  /** User-defined convention */
  case Custom(fn: String => String)
}

object CaseConvention {
  /** Transform a string according to specific case convention */
  def transform(cc: CaseConvention, str: String): String = cc match {
    case NoConvention => str
    case Custom(fn) => fn(str)
    case CamelCase => parseWords(str).zipWithIndex.map(camelize).mkString
    case SnakeCase => parseWords(str).mkString("_")
    case ScreamingSnakeCase => parseWords(str).map(_.toUpperCase).mkString("_")
    case KebabCase => parseWords(str).mkString("-")
    case CapitalizedKebabCase => parseWords(str).map(_.capitalize).mkString("-")
    case PascalCase => parseWords(str).map(_.capitalize).mkString
  }

  /** @return All words except first capitalized */
  private def camelize(data: (String, Int)): String =
    if (data._2 == 0) data._1
    else data._1.capitalize

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
}
