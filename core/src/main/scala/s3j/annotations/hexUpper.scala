package s3j.annotations

import scala.annotation.StaticAnnotation

/** When applied to a binary field, causes this field to be serialized as uppercase hexadecimal */
class hexUpper extends StaticAnnotation
