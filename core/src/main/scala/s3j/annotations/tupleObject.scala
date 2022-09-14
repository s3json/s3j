package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When applied to case class, causes serializer to output tuple (array) instead of object. `@restFields` annotation
 * on `JsArray` parameter could be used to append dynamic content to array.
 * ==Example:==
 * {{{
 *   @tupleObject
 *   case class Test(x: Int, y: String, z: Boolean)
 *
 *   @tupleObject
 *   case class Test2(x: Int, y: String, @restFields z: JsArray)
 * }}}
 * `Test(123, "qwe", true)` will be serialized as `[123, "qwe", true]`.
 * `Test2(123, "qwe", JsArray(123, 456))` will be serialized as `[123, "qwe", 123, 456]`.
 */
class tupleObject extends StaticAnnotation
