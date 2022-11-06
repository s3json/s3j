package s3j.annotations

import s3j.core.primitives.PrimitivesPlugin

import scala.annotation.StaticAnnotation

/** When applied to a numeric field, causes this field to be serialized as unsigned number. */
@usePlugin[PrimitivesPlugin]
class jsonUnsigned extends StaticAnnotation
