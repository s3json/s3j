package s3j.macros.generic

import scala.quoted.Quotes
import scala.collection.mutable

private[macros] object PluginCtxUtils {
  def formatAnnotation(using q: Quotes)(annotation: q.reflect.TypeRepr, typeArgs: Seq[q.reflect.TypeRepr],
                                        args: Seq[q.reflect.Term]): String =
  {
    val sb = new mutable.StringBuilder()
    appendAnnotation(sb, annotation, typeArgs, args)
    sb.result()
  }

  def appendAnnotation(using q: Quotes)(sb: mutable.StringBuilder, annotation: q.reflect.TypeRepr,
                                        typeArgs: Seq[q.reflect.TypeRepr], args: Seq[q.reflect.Term]): Unit =
  {
    import q.reflect.*
    sb ++= "@" ++= annotation.typeSymbol.fullName

    if (typeArgs.nonEmpty) {
      sb ++= "["
      typeArgs.map(_.show).addString(sb, ", ")
      sb ++= "]"
    }

    if (args.nonEmpty) {
      sb ++= "("
      args.map(_.show).addString(sb, ", ")
      sb ++= ")"
    }
  }
}
