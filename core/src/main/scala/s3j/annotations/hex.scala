package s3j.annotations

import scala.annotation.StaticAnnotation

/** When applied to a binary field, causes this field to be serialized as lowercase hexadecimal */
class hex extends StaticAnnotation
