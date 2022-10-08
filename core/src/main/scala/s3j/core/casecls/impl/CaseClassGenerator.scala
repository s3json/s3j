package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.{DecodingCode, DecodingEnvironment, GenerationEnvironment}
import s3j.core.casecls.modifiers.UnknownKeysModifier
import s3j.format.util.{DecoderUtils, ObjectFormatUtils}
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, util}
import s3j.io.{JsonReader, JsonToken, JsonWriter, KeyHandle}
import s3j.macros.GenerationContext
import s3j.macros.GenerationContext.GenerationCandidate
import s3j.macros.codegen.{CodeUtils, Variable}
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
    val keyHandle: Variable[KeyHandle] = Variable.create("key")
    val keyHash: Variable[Int] = Variable.create("keyHash")

    def matchKeys(using Quotes): Expr[Any] =
      CodeUtils.matchString(obj.result.handledKeys)(
        caseHash = _.hashCode,
        inputHash = keyHash,
        caseEquals = key => '{ $keyHandle.stringEquals(${ Expr(key) }) },
        caseCode = key => '{
          if (${keyTracker.isKeyPresent(key)}) ObjectFormatUtils.throwDuplicateKey($reader, ${ Expr(key) })
          ${ keyTracker.markKeyPresence(key) }
          ${ decoder.decodeKey(key, reader) }
        },
        fallbackCode = '{
          val keyString: String = $keyHandle.toString
          ${ generateUnknownKey(reader, decoder, 'keyString) }
        }
      )

    Variable.defineVariables(Seq(keyHandle, keyHash), '{
      while ($reader.peekToken == JsonToken.TKey) {
        ${ keyHandle := '{ $reader.key } }
        ${ keyHash := '{ $keyHandle.stringHashCode } }
        $reader.nextToken()
        ${ matchKeys }
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
      def generate(using q: Quotes)(): Expr[Any] =
        CodeUtils.makeCodec(c.generationMode)(
          encoder = generateEncoder _,
          decoder = generateDecoder _
        )
    }
}
