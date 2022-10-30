package s3j.internal.utils

import s3j.io.{JsonReader, JsonWriter}
import s3j.format.{JsonEncoder, JsonDecoder, JsonFormat}

import scala.quoted.{Expr, Quotes, Type, quotes}

object Utils {
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

  def generateCodec[T](mode: GenerationMode)(
    encoder: Quotes ?=> (Expr[JsonWriter], Expr[T]) => Expr[Any],
    decoder: Quotes ?=> Expr[JsonReader] => Expr[T]
  )(using Quotes, Type[T]): Expr[Any] = mode match {
    case GenerationMode.Format => '{
      new JsonFormat[T] {
        def encode(writer: JsonWriter, value: T): Unit = ${ encoder('writer, 'value) }
        def decode(reader: JsonReader): T = ${ decoder('reader) }
      }
    }

    case GenerationMode.Encoder => '{
      new JsonEncoder[T] {
        def encode(writer: JsonWriter, value: T): Unit = ${ encoder('writer, 'value) }
      }
    }

    case GenerationMode.Decoder => '{
      new JsonDecoder[T] {
        def decode(reader: JsonReader): T = ${ decoder('reader) }
      }
    }
  }
}
