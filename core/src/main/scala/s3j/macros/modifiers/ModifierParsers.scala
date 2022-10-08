package s3j.macros.modifiers

import s3j.macros.modifiers.ModifierParser.AnnotationModifier

import scala.quoted.quotes

object ModifierParsers {
  /** @return Parse function for modifier with single string argument */
  def parseString(f: String => Modifier): ModifierParser.ParseFunction = {
    case ann: AnnotationModifier =>
      import quotes.reflect.*
      ann.args match {
        case arg :: Nil if arg.isExprOf[String] => f(arg.asExprOf[String].valueOrAbort)
        case _ => report.errorAndAbort("@key annotation must contain a string literal as argument")
      }
  }
}
