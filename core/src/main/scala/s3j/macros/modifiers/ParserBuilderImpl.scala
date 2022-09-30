package s3j.macros.modifiers

import s3j.macros.modifiers.ModifierParser.{ParserBuilder, StoredModifier}
import s3j.macros.utils.QualifiedName

import scala.collection.mutable
import scala.quoted.Quotes

private[modifiers] object ParserBuilderImpl {
  // special modifier instance for PartialFunction.applyOrElse
  case object ModifierNotParsed extends Modifier {
    def key: ModifierKey[Modifier] = throw new UnsupportedOperationException
  }
}

private[modifiers] class ParserBuilderImpl extends ParserBuilder {
  private type LiftedFn = (Quotes, StoredModifier) => Option[Modifier]
  private val _parsers: mutable.Map[String, LiftedFn] = mutable.HashMap.empty

  private def lift(f: ParseFunction): LiftedFn =
    (q, m) => {
      val r = f(using q).applyOrElse(m, _ => ParserBuilderImpl.ModifierNotParsed)
      if (r != ParserBuilderImpl.ModifierNotParsed) Some(r) else None
    }

  private def liftConstant(m: Modifier): LiftedFn = (_, _) => Some(m)

  def parse[T](parser: ParseFunction)(using n: QualifiedName[T]): ParserBuilder =
    parseInner(n.name, lift(parser))

  def parse[T](modifier: Modifier)(using n: QualifiedName[T]): ParserBuilder =
    parseInner(n.name, liftConstant(modifier))

  def parse(name: String)(parser: ParseFunction): ParserBuilder =
    parseInner(name, lift(parser))

  def parse(name: String, modifier: Modifier): ParserBuilder =
    parseInner(name, liftConstant(modifier))

  private def parseInner(name: String, parser: LiftedFn): ParserBuilder = {
    _parsers.put(name, parser)
    this
  }

  def build(): ModifierParser = new ModifierParser {
    private val _finalParsers: Map[String, LiftedFn] = _parsers.toMap
    val supportedTypes: Set[String] = _finalParsers.keySet

    def apply(using q: Quotes)(mod: StoredModifier): Modifier = {
      val fn = _finalParsers.getOrElse(mod.typeName, throw new RuntimeException("No parser found for type " +
        mod.typeName + " (stored modifier " + mod + ")"))

      fn(q, mod).getOrElse(throw new RuntimeException("Parser function not matched for modifier " + mod))
    }
  }
}
