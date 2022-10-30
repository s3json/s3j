package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.{DecodingCode, DecodingEnvironment, GenerationEnvironment, ObjectField}
import s3j.core.casecls.CaseClassCode
import s3j.format.util.ObjectFormatUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter, KeyHandle}
import s3j.macros.codegen.{CodeUtils, Variable}
import s3j.macros.generic.GenerationMode

import scala.quoted.{Expr, Quotes, Type, quotes}

private[casecls] class CaseClassCodeGenerator[T](field: ObjectField[T])(using Type[T]) {
  private def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit] = {
    given env: GenerationEnvironment with {}
    '{
      val innerWriter = ObjectFormatUtils.writeBeginObject($writer)
      ${ field.generateEncoder('{ innerWriter }, value) }
      ObjectFormatUtils.writeEndObject($writer)
    }
  }

  private def generateUnknownKey(reader: Expr[JsonReader], decoder: DecodingCode, key: Expr[String])
                                (using Quotes, DecodingEnvironment): Expr[Any] =
  {
    if (field.handlesDynamicKeys) decoder.decodeDynamicKey(key, reader)
    else '{ ObjectFormatUtils.throwUnknownKey($reader, $key) }
  }

  private def generateDecodingLoop(reader: Expr[JsonReader], decoder: DecodingCode, keyTracker: KeyPresenceTracker)
                                  (using Quotes, DecodingEnvironment): Expr[Unit] =
  {
    val keyHandle: Variable[KeyHandle] = Variable.create("key")
    val keyHash: Variable[Int] = Variable.create("keyHash")

    def matchKeys(using Quotes): Expr[Any] =
      CodeUtils.matchString(field.handledKeys)(
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
    val keyTracker = new KeyPresenceTracker(field.handledKeys)
    given env: DecodingEnvironment with {
      def isKeyPresent(key: String)(using Quotes): Expr[Boolean] = keyTracker.isKeyPresent(key)
      def checkRequiredKey(key: String)(using Quotes): Expr[Unit] =
        '{ if (!${ isKeyPresent(key) }) ObjectFormatUtils.throwMissingKey($reader, ${Expr(key)}) }
    }

    val decoder = field.generateDecoder

    Variable.defineVariables(keyTracker.variables ++ decoder.usedVariables, '{
      val innerReader = ObjectFormatUtils.expectBeginObject($reader)
      ${ generateDecodingLoop('innerReader, decoder, keyTracker) }
      ObjectFormatUtils.expectEndObject($reader)
      ${ decoder.decodeFinal() }
      ${ decoder.decodeResult().asExprOf[T] }
    })
  }

  val result: CaseClassCode[T] = new CaseClassCode[T] {
    def encoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit] =
      generateEncoder(writer, value)

    def decoder(reader: Expr[JsonReader])(using Quotes): Expr[T] =
      generateDecoder(reader)

    def format(mode: GenerationMode)(using Quotes): Expr[Any] =
      CodeUtils.makeCodec(mode)(
        encoder = generateEncoder _,
        decoder = generateDecoder _
      )
  }
}
