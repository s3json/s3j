package s3j.pekko

import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, Marshalling, ToEntityMarshaller}
import org.apache.pekko.http.scaladsl.model.{ContentTypes, HttpEntity, RequestEntity}
import org.apache.pekko.http.scaladsl.unmarshalling.{FromRequestUnmarshaller, Unmarshaller}
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.io.{StreamJsonReader, StreamJsonWriter}

import java.io.{InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

object PekkoMarshallers {
  // TODO: Stub implementations with full buffering for now, migrate to loom later:

  def encodeEntity[T](data: T, indent: Int = 0)(using JsonEncoder[_ >: T], ExecutionContext, Materializer): RequestEntity = {
    val builder = ByteString.newBuilder
    val writer = new StreamJsonWriter(new OutputStreamWriter(builder.asOutputStream, StandardCharsets.UTF_8), indent)
    implicitly[JsonEncoder[_ >: T]].encode(writer, data)
    writer.close()
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
      val ret = Marshalling.WithFixedContentType(ContentTypes.`application/json`, () => encodeEntity(data)) :: Nil
      Future.successful(ret)
    }

  given fromRequestUnmarshaller[T](using JsonDecoder[_ <: T], Materializer): FromRequestUnmarshaller[T] =
    Unmarshaller { implicit ec => request =>
      decodeEntity(request.entity)
    }
}
