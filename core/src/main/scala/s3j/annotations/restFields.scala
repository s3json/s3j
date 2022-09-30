package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When applied to a parameter of type `JsObject`, causes that parameter to serve as a place for all unmatched fields.
 * Undoes effect of [[failUnknownKeys]].
 *
 * ==Example usage:==
 * {{{
 *  case class Test(foo: Int, bar: String, @restFields rest: JsObject)
 * }}}
 * All fields except `foo` and `bar` will be stored in `rest` object. Likewise, on serialization, everything from `rest`
 * object will be appended to serialized output.
 */
class restFields extends StaticAnnotation
