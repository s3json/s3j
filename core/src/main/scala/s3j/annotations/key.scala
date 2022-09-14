package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When applied to a parameter, overrides parameter's key in serialized form.
 * ==Example:==
 * {{{
 *   case class Test(@key("bar") foo: Int)
 * }}}
 * `foo` will be serialized as `bar`.
 * 
 * @param k Key to use
 */
class key(k: String) extends StaticAnnotation
