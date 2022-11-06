package s3j.macros.codegen

import s3j.io.{JsonReader, JsonWriter}
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, StringyDecoder}
import s3j.macros.generic.GenerationMode

import scala.quoted.*

object CodeUtils {
  def placeholderValue[T](using Quotes, Type[T]): Expr[T] =
    (Type.of[T] match {
      case '[ AnyRef ]    => '{ null }
      case '[ Boolean ]   => Expr(false)
      case '[ Byte ]      => Expr(0.toByte)
      case '[ Short ]     => Expr(0.toShort)
      case '[ Int ]       => Expr(0)
      case '[ Long ]      => Expr(0L)
      case '[ Char ]      => Expr('\u0000')
      case '[ Float ]     => Expr(0.0F)
      case '[ Double ]    => Expr(0.0D)
      case '[ Unit ]      => '{ () }
      case _ => quotes.reflect.report.errorAndAbort("Unsupported type for placeholder: " + Type.show[T])
    }).asInstanceOf[Expr[T]]

  /** Place all statements into a single block, using last block as a result */
  def joinBlocks[T](blocks: Seq[Expr[T]])(using Quotes, Type[T]): Expr[T] = {
    if (blocks.isEmpty) {
      throw new IllegalAccessException("joinBlocks with empty blocks (at least one is needed for result)")
    }

    joinBlocks(blocks.init, blocks.last)
  }

  /** Place all statements into a single block, removing nested blocks for readability of generated code */
  def joinBlocks[T](blocks: Seq[Expr[Any]], result: Expr[T])(using q: Quotes, t: Type[T]): Expr[T] = {
    import q.reflect.*
    val ret = List.newBuilder[Statement]

    def processStatement(s: Statement): Unit =
      s match {
        case Block(code, result) =>
          for (s <- code) processStatement(s)
          processStatement(result)

        case _ => ret += s
      }

    for (b <- blocks) processStatement(b.asTerm)

    val finalResult = result.asTerm match {
      case Block(code, r) =>
        for (s <- code) processStatement(s)
        r

      case other => other
    }

    Block(ret.result(), finalResult).asExprOf[T]
  }

  /** Build a codec object using generation mode and provided parts */
  def makeCodec[T](mode: GenerationMode)(
    encoder: Quotes ?=> (writer: Expr[JsonWriter], value: Expr[T]) => Expr[Any],
    decoder: Quotes ?=> (reader: Expr[JsonReader]) => Expr[T]
  )(using Quotes, Type[T]): Expr[Any] = mode match {
    case GenerationMode.Decoder => '{
      new JsonDecoder[T] {
        def decode(reader: JsonReader): T = ${ decoder('reader) }
        override def toString: String = ${ Expr("JsonDecoder.derived[" + Type.show[T] + "]") }
      }
    }

    case GenerationMode.Encoder => '{
      new JsonEncoder[T] {
        def encode(writer: JsonWriter, value: T): Unit = ${ encoder('writer, 'value) }
        override def toString: String = ${ Expr("JsonEncoder.derived[" + Type.show[T] + "]") }
      }
    }

    case GenerationMode.Format => '{
      new JsonFormat[T] {
        def decode(reader: JsonReader): T = ${ decoder('reader) }
        def encode(writer: JsonWriter, value: T): Unit = ${ encoder('writer, 'value) }
        override def toString: String = ${ Expr("JsonFormat.derived[" + Type.show[T] + "]") }
      }
    }

    case GenerationMode.StringDecoder | GenerationMode.StringEncoder | GenerationMode.StringFormat =>
      throw new IllegalArgumentException("Stringy generation modes are not supported by makeCodec")

    case GenerationMode.Schema => throw new IllegalArgumentException("Schema generation is not supported by makeCodec")
  }

  /** Build a hash-code based string match code */
  def matchString[T](cases: Iterable[T])(
    caseHash: T => Int,
    inputHash: Expr[Int],
    caseEquals: Quotes ?=> T => Expr[Boolean],
    caseCode: Quotes ?=> T => Expr[Any],
    fallbackCode: Quotes ?=> Expr[Any]
  )(using q: Quotes): Expr[Any] = {
    import q.reflect.*
    val caseDefs = Vector.newBuilder[CaseDef]
    val matched = Variable.create[Boolean]("matched")

    def caseCodeWrapper(c: T): Term = '{
      if (${ caseEquals(c) }) {
        ${ matched := '{ true} }
        ${ caseCode(c) }
      }
    }.asTerm

    for ((h, cs) <- cases.groupBy(caseHash)) {
      val code: Term =
        if (cs.size == 1) caseCodeWrapper(cs.head)
        else Block(cs.map(caseCodeWrapper).toList, Literal(UnitConstant()))

      caseDefs += CaseDef(Literal(IntConstant(h)), None, code)
    }

    caseDefs += CaseDef(Wildcard(), None, Literal(UnitConstant()))

    Variable.defineVariables(Seq(matched), '{
      ${ Match(inputHash.asTerm, caseDefs.result().toList).asExpr }
      if (!$matched) $fallbackCode
    })
  }
}
