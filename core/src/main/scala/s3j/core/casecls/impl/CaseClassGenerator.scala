package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.{DecodingCode, DecodingEnvironment, GenerationEnvironment}
import s3j.core.casecls.modifiers.UnknownKeysModifier
import s3j.format.util.{DecoderUtils, ObjectFormatUtils}
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, util}
import s3j.io.{JsonReader, JsonToken, JsonWriter, KeyHandle}
import s3j.macros.GenerationContext
import s3j.macros.GenerationContext.GenerationCandidate
import s3j.macros.codegen.Variable
import s3j.macros.generic.GenerationMode
import s3j.macros.modifiers.ModifierSet

import scala.quoted.{Expr, Quotes, Type, quotes}

private[casecls] class CaseClassGenerator[T](modifiers: ModifierSet)(using c: GenerationContext)(using Quotes, Type[T]) {
  import quotes.reflect.*

  private val obj = new CaseClassObjectBuilder[T](
    CaseClassObjectBuilder.StackEntry(
      t = Type.of[T],
      modifiers = modifiers,
      path = Nil,
      label = None
    ) :: Nil
  )

  private def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit] = {
    given env: GenerationEnvironment with { export c.freshName }
    '{
      val innerWriter = ObjectFormatUtils.writeBeginObject($writer)
      ${obj.result.generateEncoder('{ innerWriter }, value)}
      ObjectFormatUtils.writeEndObject($writer)
    }
  }

  private def generateUnknownKey(reader: Expr[JsonReader], decoder: DecodingCode, key: Expr[String])
                                (using Quotes, DecodingEnvironment): Expr[Any] =
  {
    if (obj.result.handlesDynamicKeys) decoder.decodeDynamicKey(key, reader)
    else {
      val allowUnknownKeys = modifiers.get(UnknownKeysModifier.key).fold(false)(_.allow)
      if (allowUnknownKeys) '{ DecoderUtils.skipValue($reader) }
      else '{ ObjectFormatUtils.throwUnknownKey($reader, $key) }
    }
  }

  private def generateDecodingLoop(reader: Expr[JsonReader], decoder: DecodingCode, keyTracker: KeyPresenceTracker)
                                  (using Quotes, DecodingEnvironment): Expr[Unit] =
  {
    val keyFound: Variable[Boolean] = Variable.create("keyFound")
    val keyHandle: Variable[KeyHandle] = Variable.create("key")
    val keyHash: Variable[Int] = Variable.create("keyHash")
    val keys: IndexedSeq[(String, Int)] = obj.result.handledKeys.toVector.map(s => (s, s.hashCode)).sortBy(_._2)

    def generateNode(low: Int, high: Int)(using Quotes): Expr[Unit] = {
      if (low == high) {
        val key = keys(low)._1
        val keyExpr = Expr(key)
        '{
          if ($keyHandle.stringEquals($keyExpr)) {
            if (${ keyTracker.isKeyPresent(key) }) ObjectFormatUtils.throwDuplicateKey($reader, $keyExpr)
            ${ keyFound := Expr(true) }
            ${ keyTracker.markKeyPresence(key) }
            ${ decoder.decodeKey(key, reader) }
          }
        }
      } else if (low + 1 == high) {
        val splitHash = keys(high)._2
        '{ if ($keyHash < ${ Expr(splitHash) }) ${generateNode(low, low)} else ${generateNode(high, high)} }
      } else {
        val mid = (low + high) / 2
        val splitHash = keys(mid)._2
        '{ if ($keyHash < ${ Expr(splitHash) }) ${generateNode(low, mid - 1)} else ${generateNode(mid, high)} }
      }
    }

    Variable.defineVariables(Seq(keyFound, keyHandle, keyHash), '{
      while ($reader.peekToken == JsonToken.TKey) {
        ${ keyFound := Expr(false) }
        ${ keyHandle := '{ $reader.key } }
        ${ keyHash := '{ $keyHandle.stringHashCode } }
        $reader.nextToken()
        ${ generateNode(0, keys.size - 1) }
        if (!$keyFound) {
          val keyString: String = $keyHandle.toString
          ${ generateUnknownKey(reader, decoder, 'keyString) }
        }
      }
    })
  }

  private def generateDecoder(reader: Expr[JsonReader])(using Quotes): Expr[T] = {
    val keyTracker = new KeyPresenceTracker(obj.result.handledKeys, c)
    given env: DecodingEnvironment with {
      export c.freshName

      def isKeyPresent(key: String)(using Quotes): Expr[Boolean] = keyTracker.isKeyPresent(key)
      def checkRequiredKey(key: String)(using Quotes): Expr[Unit] =
        '{ if (!${ isKeyPresent(key) }) ObjectFormatUtils.throwMissingKey($reader, ${Expr(key)}) }
    }

    val decoder = obj.result.generateDecoder

    Variable.defineVariables(keyTracker.variables ++ decoder.usedVariables, '{
      val innerReader = ObjectFormatUtils.expectBeginObject($reader)
      ${ generateDecodingLoop('innerReader, decoder, keyTracker) }
      ObjectFormatUtils.expectEndObject($reader)
      ${ decoder.decodeFinal() }
      ${ decoder.decodeResult().asExprOf[T] }
    })
  }

  val candidate: GenerationCandidate =
    new GenerationCandidate {
      def confidence: Option[Int] = Some(500)
      def identity: AnyRef = obj.result.identity

      // Quasiquotes were much better than _this_:
      def generate(using q: Quotes)(): Expr[Any] = c.generationMode match {
        case GenerationMode.Decoder => '{
          new JsonDecoder[T] {
            def decode(reader: JsonReader): T = ${ generateDecoder('reader) }
          }
        }

        case GenerationMode.Encoder => '{
          new JsonEncoder[T] {
            def encode(writer: JsonWriter, value: T): Unit = ${ generateEncoder('writer, 'value) }
          }
        }

        case GenerationMode.Format => '{
          new JsonFormat[T] {
            def decode(reader: JsonReader): T = ${ generateDecoder('reader) }
            def encode(writer: JsonWriter, value: T): Unit = ${ generateEncoder('writer, 'value) }
          }
        }
      }
    }
}
