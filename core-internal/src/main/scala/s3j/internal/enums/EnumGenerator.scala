package s3j.internal.enums

import s3j.format.util.DecoderUtils
import s3j.internal.utils.{CaseConvention, GenerationMode, Utils}
import s3j.io.{JsonReader, JsonWriter}

import scala.quoted.{Expr, Quotes, Type, quotes}

object EnumGenerator {
  def isEnum[T](using Quotes, Type[T]): Boolean = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol
    sym.flags.is(Flags.Sealed) && sym.flags.is(Flags.Enum)
  }
}

class EnumGenerator[T](mode: GenerationMode)(using q: Quotes, tt: Type[T]) {
  import q.reflect.*

  private val typeSymbol: Symbol = TypeRepr.of[T].typeSymbol

  private class EnumCase(val sym: Symbol, val ordinal: Int) {
    type C <: T
    given caseType: Type[C] = sym.typeRef.asType.asInstanceOf[Type[C]]

    val singleton: Boolean = !sym.isClassDef
    val customCase: Boolean = !singleton && sym.name == "Custom"

    def singletonValue(using Quotes): Expr[C] = {
      import quotes.reflect.*
      Select(Ident(typeSymbol.companionModule.termRef.asInstanceOf[TermRef]), sym.asInstanceOf[Symbol]).asExprOf[C]
    }

    val customValue: Symbol =
      if (customCase) sym.memberField(sym.primaryConstructor.paramSymss.head.head.name)
      else Symbol.noSymbol

    val discriminator: String = {
      val ann: Option[String] = sym.annotations.flatMap(parseDiscriminator).headOption
      ann.getOrElse(CaseConvention.transform(CaseConvention.KebabCase, sym.name))
    }

    override def toString: String =
      s"EnumCase(sym=${sym.fullName}, singleton=$singleton, customCase=$customCase, discriminator='$discriminator')"
  }

  private val cases: Seq[EnumCase] = typeSymbol.children.zipWithIndex.map { case (s, i) => new EnumCase(s, i) }

  private def parseDiscriminator(t: Tree): Option[String] =
    t match {
      case Apply(Select(New(tpe), _), List(arg)) if tpe.tpe.typeSymbol.fullName == "s3j.annotations.discriminator" =>
        Some(arg.asExprOf[String].valueOrAbort)

      case _ => None
    }

  private def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val matchCases = Vector.newBuilder[CaseDef]

    for (c <- cases) {
      val handler: Expr[Any] =
        if (c.customCase) {
          val customValue: Expr[String] = Select(
            TypeApply(Select.unique(value.asTerm, "asInstanceOf"), TypeTree.of[c.C] :: Nil),
            c.customValue.asInstanceOf[Symbol]
          ).asExprOf[String]

          '{ $writer.stringValue($customValue) }
        } else if (c.singleton) '{ $writer.stringValue(${ Expr(c.discriminator) }) }
        else report.errorAndAbort("Unsupported case: " + c)

      matchCases += CaseDef(Literal(IntConstant(c.ordinal)), None, handler.asTerm)
    }

    Match(Select.unique(value.asTerm, "ordinal"), matchCases.result().toList).asExpr
  }

  private def generateDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] = {
    val maxLength: Int =
      if (!cases.exists(_.customCase)) cases.map(_.discriminator.length).max
      else 128

    val possibleCases: String = cases.map(c => "'" + c.discriminator + "'").mkString(", ")
    val casesMessage: String =
      if (cases.exists(_.customCase)) possibleCases + " or custom value"
      else possibleCases

    def generateMatch(str: Expr[String])(using Quotes): Expr[T] = {
      import quotes.reflect.*
      val matchCases = Vector.newBuilder[CaseDef]

      for (c <- cases if !c.customCase)
        matchCases += CaseDef(Literal(IntConstant(c.discriminator.hashCode)),
          Some('{ $str == ${Expr(c.discriminator)} }.asTerm), c.singletonValue.asTerm)

      for (c <- cases.find(_.customCase)) {
        val customCtor: Expr[T] = Apply(
          Select(New(TypeTree.of[c.C]), TypeRepr.of[c.C].typeSymbol.primaryConstructor),
          str.asTerm :: Nil
        ).asExprOf[T]

        matchCases += CaseDef(Wildcard(), None, customCtor.asTerm)
      }

      Match('{ $str.hashCode }.asTerm, matchCases.result().toList).asExprOf[T]
    }

    '{
      val str = DecoderUtils.decodeStringRaw($reader, ${ Expr(maxLength) })
      if (str == null) DecoderUtils.throwInvalidEnumConstant($reader, str, ${ Expr(casesMessage) })
      ${ generateMatch('str) }
    }
  }

  val result: Expr[Any] = Utils.generateCodec(mode)(
    encoder = generateEncoder _,
    decoder = generateDecoder _
  )
}
