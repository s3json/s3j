package s3j.core.enums

import s3j.annotations.naming.CaseConvention
import s3j.core.enums.modifiers.{DiscriminatorModifier, EnumCaseModifier}
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.GenerationContext
import s3j.macros.GenerationContext.GenerationCandidate
import s3j.macros.codegen.{CodeUtils, Variable}
import s3j.macros.modifiers.ModifierSet

import scala.quoted.{Expr, Quotes, Type}

private[enums] class StringyCandidate[T](modifiers: ModifierSet)(using q: Quotes, c: GenerationContext, t: Type[T])
extends GenerationCandidate {
  import q.reflect.*

  private case class EnumCase(sym: Symbol, discriminator: String, ordinal: Int)
  private case class EnumStringIdentity(keys: Seq[String])

  private val typeSymbol: Symbol = TypeRepr.of[T].typeSymbol
  private val cases: Seq[EnumCase] = typeSymbol.children
    .zipWithIndex // assume that ordinal equals to position in `children` array
    .map { (sym, ord) =>
      val mods = ModifierSet.inherit(modifiers, c.symbolModifiers(sym).own)
      val discriminator = mods.get(DiscriminatorModifier.key).map(_.name).getOrElse {
        val cc = mods.get(EnumCaseModifier.key).fold(CaseConvention.NoConvention)(_.value)
        CaseConvention.transform(cc, sym.name)
      }
      EnumCase(sym, discriminator, ord)
    }

  private def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any] = {
    Match(Select.unique(value.asTerm, "ordinal"), cases
      .map(c => {
        val encoder = '{ $writer.value(${ Expr(c.discriminator) }) }
        CaseDef(Literal(IntConstant(c.ordinal)), None, encoder.asTerm)
      })
      .toList).asExpr
  }

  private def generateDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] = {
    val result = Variable.create[T]("result")
    val maxLength = Expr(cases.map(_.discriminator.length).max)
    val errorString = Expr("invalid enum constant: possible values are " +
      cases.map(c => s"'${c.discriminator}'").mkString(", "))

    def matchCases(caseName: Expr[String], hash: Expr[Int])(using Quotes): Expr[Any] =
      CodeUtils.matchString(cases)(
        caseHash = _.discriminator.hashCode,
        inputHash = hash,
        caseEquals = c => '{ $caseName == ${ Expr(c.discriminator) } },
        caseCode = c => result := Select(Ident(typeSymbol.companionModule.termRef), c.sym).asExprOf[T],
        fallbackCode = '{ $reader.parseError($errorString) }
      )

    Variable.defineVariables(Seq(result), '{
      val name = DecoderUtils.decodeString($reader, $maxLength, $errorString)
      val nameHash = name.hashCode
      ${ matchCases('name, 'nameHash) }
      $result
    }).asExprOf[T]
  }

  val confidence: Option[Int] = Some(500)
  val identity: AnyRef = EnumStringIdentity(cases.map(_.discriminator).toVector)
  def generate(using Quotes)(): Expr[Any] = CodeUtils.makeCodec(c.generationMode)(
    encoder = generateEncoder _,
    decoder = generateDecoder _
  )
}
