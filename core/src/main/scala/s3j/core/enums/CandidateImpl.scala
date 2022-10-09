package s3j.core.enums

import s3j.core.casecls.modifiers.UnknownKeysModifier
import s3j.core.enums.modifiers.EnumObjectModifier.Behavior
import s3j.core.enums.modifiers.{AllowBufferingModifier, DiscriminatorFieldModifier, EnumObjectModifier}
import s3j.format.util
import s3j.format.util.{DecoderUtils, ObjectFormatUtils}
import s3j.io.{JsonReader, JsonToken, JsonWriter}
import s3j.macros.GenerationContext
import s3j.macros.GenerationContext.GenerationCandidate
import s3j.macros.codegen.{CodeUtils, Variable}
import s3j.macros.modifiers.ModifierSet
import s3j.macros.traits.NestedResult

import scala.annotation.threadUnsafe
import scala.quoted.{Expr, Quotes, Type, quotes}

private[enums] class CandidateImpl[T](modifiers: ModifierSet)(using q: Quotes, c: GenerationContext, t: Type[T])
extends GenerationCandidate {
  import q.reflect.*

  private case class EnumCase(sym: Symbol, mods: ModifierSet, ordinal: Int, singleton: Boolean, discriminator: String) {
    type C <: T
    given caseType: Type[C] = sym.typeRef.asType.asInstanceOf[Type[C]]

    @threadUnsafe
    lazy val nested: NestedResult[C] = c.nested[C].modifiers(mods).build()

    def singletonValue(using Quotes): Expr[C] = {
      import quotes.reflect.*
      Select(Ident(typeSymbol.companionModule.termRef.asInstanceOf[TermRef]), sym.asInstanceOf[Symbol]).asExprOf[C]
    }

    def decode(reader: Expr[JsonReader])(using Quotes): Expr[C] =
      if (singleton) {
        def remainingFields(r: Expr[JsonReader])(using Quotes): Expr[Any] =
          if (allowUnknownKeys) '{ ObjectFormatUtils.skipRemainingFields($r) }
          else '{ ObjectFormatUtils.failRemainingFields($r) }

        '{
          val r = ObjectFormatUtils.expectBeginObject($reader)
          ${ remainingFields('r) }
          ObjectFormatUtils.expectEndObject($reader)
          $singletonValue
        }
      } else '{ ${nested.decoder}.decode($reader) }
  }

  private case class EnumCaseIdentity(discriminator: String)
  private case class EnumIdentity(behavior: Behavior, cases: Set[EnumCaseIdentity])

  private val typeSymbol: Symbol = TypeRepr.of[T].typeSymbol
  private val behavior: Behavior = modifiers.get(EnumObjectModifier.key).fold(Behavior.Default)(_.behavior)
  private val allowUnknownKeys: Boolean = modifiers.get(UnknownKeysModifier.key).exists(_.allow)
  private val discriminatorField: String = modifiers.get(DiscriminatorFieldModifier.key).fold("type")(_.name)

  private val cases: Seq[EnumCase] = typeSymbol.children
    .zipWithIndex
    .map { (sym, i) =>
      val mods = ModifierSet.inherit(modifiers, c.symbolModifiers(sym).own)
      val discriminator = EnumUtils.discriminator(sym, mods)
      EnumCase(sym, mods, i, !sym.isClassDef, discriminator)
    }

  val confidence: Option[Int] = Some(500)
  val identity: AnyRef = EnumIdentity(behavior, cases
    .map(c => EnumCaseIdentity(c.discriminator))
    .toSet)

  private def encodeCase(c: EnumCase)(writer: Expr[JsonWriter], value: Expr[c.C])(using Quotes): Expr[Any] = {
    val asString = c.singleton && (behavior match {
      case Behavior.Default => cases.forall(_.singleton)
      case Behavior.AllowStrings => true
      case Behavior.ForceObject => false
    })

    if (asString)
      return '{ $writer.value(${ Expr(c.discriminator) }) }

    def body(writer: Expr[JsonWriter])(using Quotes): Expr[Any] =
      if (c.singleton) '{}
      else '{ ${c.nested.encoder}.encode($writer, $value) }

    '{
      val innerWriter = ObjectFormatUtils.encodeDiscriminator($writer, ${ Expr(discriminatorField) },
        ${ Expr(c.discriminator) })

      ${ body('innerWriter) }
      ObjectFormatUtils.writeEndObject($writer)
    }
  }

  // match when no ordinal method is available:
  private def encodeMatchGeneric(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    Match(
      value.asTerm,
      cases
        .map {
          case c if c.singleton =>
            val outerIdent = Ident(typeSymbol.companionModule.asInstanceOf[Symbol].termRef)
            CaseDef(Select(outerIdent, c.sym.asInstanceOf[Symbol]), None, encodeCase(c)(writer, null).asTerm)

          case c =>
            val caseSym = c.sym.asInstanceOf[Symbol] // cast to Symbol in this Quotes universe
            val bindSym = Symbol.newBind(Symbol.spliceOwner, "v", Flags.EmptyFlags, caseSym.typeRef)
            CaseDef(Bind(bindSym, Typed(Wildcard(), TypeIdent(caseSym))), None,
              encodeCase(c)(writer, Ident(bindSym.termRef).asExprOf[c.C]).asTerm)
        }
        .toList
    ).asExpr
  }

  private def encodeMatchOrdinal(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    Match(
      Select.unique(value.asTerm, "ordinal"),
      cases
        .map {
          case c if c.singleton =>
            CaseDef(Literal(IntConstant(c.ordinal)), None, encodeCase(c)(writer, null).asTerm)

          case c =>
            val caseExpr = TypeApply(Select.unique(value.asTerm, "asInstanceOf"), List( TypeTree.of[c.C] ))
            CaseDef(Literal(IntConstant(c.ordinal)), None, encodeCase(c)(writer, caseExpr.asExprOf[c.C]).asTerm)
        }
        .toList
    ).asExpr
  }

  private def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any] = {
    if (typeSymbol.flags.is(Flags.Enum)) encodeMatchOrdinal(writer, value)
    else encodeMatchGeneric(writer, value)
  }

  private def generateObjectDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] = {
    val casesMessage = Expr(cases.map(c => "'" + c.discriminator + "'").mkString(", "))
    val result = Variable.create[T]("result")

    def matchCase(reader: Expr[JsonReader], name: Expr[String], hash: Expr[Int])(using Quotes): Expr[Any] =
      CodeUtils.matchString(cases)(
        caseHash = _.discriminator.hashCode,
        inputHash = hash,
        caseEquals = c => '{ $name == ${ Expr(c.discriminator) } },
        caseCode = c => result := c.decode(reader),
        fallbackCode = '{ DecoderUtils.throwInvalidEnumConstant($reader, $name, $casesMessage) }
      )

    Variable.defineVariables(Seq(result), '{
      val d = ObjectFormatUtils.decodeDiscriminator($reader, ${ Expr(discriminatorField) },
        ${ Expr(cases.map(_.discriminator.length).max) }, ${ Expr(modifiers.contains(AllowBufferingModifier.key)) })
      if (d == null) DecoderUtils.throwInvalidEnumConstant($reader, null, $casesMessage)
      val hash = d.discriminator.hashCode
      ${ matchCase('{ d.reader }, '{ d.discriminator }, 'hash) }
      ObjectFormatUtils.expectEndObject($reader)
      $result
    })
  }

  private def generateStringDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] = {
    val result = Variable.create[T]("result")
    val maxLength = cases.filter(_.singleton).map(_.discriminator.length).max
    val casesMessage = Expr(cases.filter(_.singleton).map(c => "'" + c.discriminator + "'").mkString(", "))

    def matchCases(str: Expr[String], hash: Expr[Int])(using Quotes): Expr[Any] =
      CodeUtils.matchString(cases.filter(_.singleton))(
        caseHash = _.discriminator.hashCode,
        inputHash = hash,
        caseEquals = c => '{ $str == ${ Expr(c.discriminator) } },
        caseCode = c => result := c.singletonValue,
        fallbackCode = '{ DecoderUtils.throwInvalidEnumConstant($reader, $str, $casesMessage) }
      )

    Variable.defineVariables(Seq(result), '{
      val str = DecoderUtils.decodeStringRaw($reader, ${ Expr(maxLength) })
      if (str == null) DecoderUtils.throwInvalidEnumConstant($reader, null, $casesMessage)

      val hash = str.hashCode
      ${ matchCases('str, 'hash) }
      $result
    })
  }

  private def generateMixedDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] = '{
    $reader.peekToken match {
      case JsonToken.TObjectStart => ${ generateObjectDecoder(reader) }
      case JsonToken.TString | JsonToken.TStringContinued => ${ generateStringDecoder(reader) }
      case _ => DecoderUtils.throwUnexpected($reader, "string or object", $reader.nextToken())
    }
  }

  private def generateDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] =
    behavior match {
      case Behavior.Default =>
        if (cases.forall(_.singleton)) generateStringDecoder(reader)
        else generateObjectDecoder(reader)

      case Behavior.AllowStrings =>
        if (cases.forall(_.singleton)) generateStringDecoder(reader)
        else if (cases.exists(_.singleton)) generateMixedDecoder(reader)
        else generateObjectDecoder(reader)

      case Behavior.ForceObject => generateObjectDecoder(reader)
    }

  def generate(using Quotes)(): Expr[Any] = CodeUtils.makeCodec(c.generationMode)(
    encoder = generateEncoder _,
    decoder = generateDecoder _
  )
}
