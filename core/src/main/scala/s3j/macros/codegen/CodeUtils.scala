package s3j.macros.codegen

import s3j.io.{JsonReader, JsonWriter}
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.macros.generic.GenerationMode

import scala.quoted.*

object CodeUtils {
  def placeholderValue[T](using Quotes, Type[T]): Expr[T] =
    (Type.of[T] match {
      case '[ AnyRef ]  => '{ null }
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
  }

  /** Build a hash-code based string match code */
  def matchString[T](cases: Iterable[T])(
    caseHash: T => Int,
    inputHash: Expr[Int],
    caseEquals: Quotes ?=> T => Expr[Boolean],
    caseCode: Quotes ?=> T => Expr[Any],
    fallbackCode: Quotes ?=> Expr[Any]
  )(using Quotes, NameGenerator): Expr[Any] = {
    val matched: Variable[Boolean] = Variable.create("matched")('{ false })
    val sortedCases = cases.toVector.sortBy(caseHash)

    def generateNode(low: Int, high: Int)(using Quotes): Expr[Unit] = {
      if (low == high) {
        '{
          if (${ caseEquals(sortedCases(low)) }) {
            ${ matched := Expr(true) }
            ${ caseCode(sortedCases(low)) }
          }
        }
      } else if (low + 1 == high) {
        val splitHash = caseHash(sortedCases(high))
        '{ if ($inputHash < ${ Expr(splitHash) }) ${generateNode(low, low)} else ${generateNode(high, high)} }
      } else {
        val mid = (low + high) / 2
        val splitHash = caseHash(sortedCases(mid))
        '{ if ($inputHash < ${ Expr(splitHash) }) ${generateNode(low, mid - 1)} else ${generateNode(mid, high)} }
      }
    }

    Variable.defineVariables(Seq(matched), '{
      ${ generateNode(0, cases.size - 1) }
      if (!$matched) ${ fallbackCode }
    })
  }
}
