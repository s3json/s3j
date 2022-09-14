package s3j.format

// Empty object is required so we could extend it later
object JsonFormat

trait JsonFormat[T] extends JsonEncoder[T] with JsonDecoder[T] {

}
