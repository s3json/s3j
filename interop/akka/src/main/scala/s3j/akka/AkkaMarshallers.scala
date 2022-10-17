package s3j.akka

import akka.http.scaladsl.marshalling.{Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, RequestEntity}
import akka.http.scaladsl.unmarshalling.{FromRequestUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import akka.util.ByteString
import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.io.{StreamJsonReader, StreamJsonWriter}

import java.io.{ByteArrayOutputStream, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*

object AkkaMarshallers {
  // TODO: Stub implementations with full buffering for now, migrate to loom later:

  def encodeEntity[T](data: T)(using JsonEncoder[_ >: T], ExecutionContext, Materializer): Future[RequestEntity] =
    Future.successful {
      val builder = ByteString.newBuilder
      val writer = new StreamJsonWriter(new OutputStreamWriter(builder.asOutputStream, StandardCharsets.UTF_8))
      implicitly[JsonEncoder[_ >: T]].encode(writer, data)
      HttpEntity(ContentTypes.`application/json`, builder.result())
    }

  def decodeEntity[T](entity: HttpEntity)(using JsonDecoder[_ <: T], ExecutionContext, Materializer): Future[T] =
    entity.toStrict(10 second span)
      .map { ent =>
        val jsonReader = new StreamJsonReader(new InputStreamReader(ent.data.iterator.asInputStream,
          StandardCharsets.UTF_8))

        implicitly[JsonDecoder[_ <: T]].decode(jsonReader)
      }

  given toEntityMarshaller[T](using JsonEncoder[_ >: T], Materializer): ToEntityMarshaller[T] =
    Marshaller { implicit ec => data =>
      encodeEntity(data).map(ent => Marshalling.WithFixedContentType(ContentTypes.`application/json`, () => ent) :: Nil)
    }

  given fromRequestUnmarshaller[T](using JsonDecoder[_ <: T], Materializer): FromRequestUnmarshaller[T] =
    Unmarshaller { implicit ec => request =>
      decodeEntity(request.entity)
    }
}
