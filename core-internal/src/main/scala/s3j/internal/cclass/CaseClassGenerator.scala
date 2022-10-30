package s3j.internal.cclass

import s3j.ast.JsObject
import s3j.format.util.ObjectFormatUtils
import s3j.format.util.ObjectFormatUtils.RestFieldsBuilder
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.internal.cclass.CaseClassModel.DecodingEnv
import s3j.internal.cclass.KeyPresenceTracker
import s3j.internal.utils.{GenerationMode, NestedFormat, Variable, Utils}
import s3j.io.{JsonReader, JsonToken, JsonWriter, KeyHandle}

import scala.quoted.{Expr, Quotes, Type, quotes}

object CaseClassGenerator {
  def isCaseClass[T](using Quotes, Type[T]): Boolean = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol
    sym.isClassDef && sym.flags.is(Flags.Case)
  }
}

class CaseClassGenerator[T](mode: GenerationMode)(using q: Quotes, tt: Type[T]) {
  import q.reflect.*

  val model = new CaseClassModelImpl[T](mode)

  private def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any] = '{
    val innerWriter = ObjectFormatUtils.writeBeginObject($writer)
    ${ model.encode('innerWriter, value) }
    ObjectFormatUtils.writeEndObject($writer)
  }

  private def generateDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] = {
    val keyPresence = new KeyPresenceTracker(model.keys)
    given decodingEnv: DecodingEnv with {
      def isKeyPresent(key: String): Expr[Boolean] = keyPresence.isKeyPresent(key)
    }

    val d = model.decode(reader)

    def generateKey(reader: Expr[JsonReader], key: Expr[KeyHandle], hash: Expr[Int])(using Quotes): Expr[Any] = {
      import quotes.reflect.*
      val cases = Vector.newBuilder[CaseDef]

      for (k <- model.keys) {
        cases += CaseDef(Literal(IntConstant(k.hashCode)), Some('{ $key.stringEquals(${ Expr(k) }) }.asTerm),
          d.staticKey(k).asTerm)
      }

      if (model.dynamicKeys) cases += CaseDef(Wildcard(), None, d.dynamicKey(key).asTerm)
      else cases += CaseDef(Wildcard(), None, '{ ObjectFormatUtils.throwUnknownKey($reader, $key.toString) }.asTerm)

      Match(hash.asTerm, cases.result().toList).asExpr
    }

    Variable.defineVariables(d.variables, '{
      val innerReader = ObjectFormatUtils.expectBeginObject($reader)
      while (innerReader.peekToken == JsonToken.TKey) {
        val key = innerReader.key
        innerReader.nextToken()

        ${ generateKey('innerReader, 'key, '{ key.stringHashCode }) }
      }
      ${ d.finalCode() }
      ObjectFormatUtils.expectEndObject($reader)
      ${ d.result() }
    })
  }

  val result: Expr[Any] = Utils.generateCodec(mode)(
    encoder = generateEncoder _,
    decoder = generateDecoder _
  )
}
