package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When applied to optional field in case class, causes this field to be serialized as `null` instead of omitting it
 * when empty. Decoding will accept both formats regardless of this annotation.
 */
class nullOption extends StaticAnnotation
