package s3j.akka

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.unmarshalling.{FromRequestUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import akka.util.ByteString
import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.io.{StreamJsonReader, StreamJsonWriter}

import java.io.{ByteArrayOutputStream, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

object AkkaMarshallers {
  // TODO: Stub implementations with full buffering for now, migrate to loom later:

  given toEntityMarshaller[T](using JsonEncoder[_ >: T]): ToEntityMarshaller[T] =
    Marshaller.withFixedContentType(ContentTypes.`application/json`) { data =>
      val builder = ByteString.newBuilder
      val writer = new StreamJsonWriter(new OutputStreamWriter(builder.asOutputStream, StandardCharsets.UTF_8))
      implicitly[JsonEncoder[_ >: T]].encode(writer, data)
      HttpEntity(ContentTypes.`application/json`, builder.result())
    }

  given fromRequestUnmarshaller[T](using JsonDecoder[_ <: T], Materializer): FromRequestUnmarshaller[T] =
    Unmarshaller { implicit ec => request =>
      request.entity.toStrict(10 second span)
        .map { ent =>
          val jsonReader = new StreamJsonReader(new InputStreamReader(ent.data.iterator.asInputStream,
            StandardCharsets.UTF_8))

          implicitly[JsonDecoder[_ <: T]].decode(jsonReader)
        }
    }
}
