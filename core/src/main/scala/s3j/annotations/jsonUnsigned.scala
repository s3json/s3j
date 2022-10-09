package s3j.annotations

import s3j.core.numbers.NumbersPlugin

import scala.annotation.StaticAnnotation

/** When applied to a numeric field, causes this field to be serialized as unsigned number. */
@usePlugin[NumbersPlugin]
class jsonUnsigned extends StaticAnnotation
