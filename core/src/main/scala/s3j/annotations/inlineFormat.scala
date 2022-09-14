package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When applied to a case class with single field, causes serializer to forget about case class existence and write that
 * field directly. No object will be emitted, and serialized contents will depend entirely on field's serializer.
 * ==Example:==
 * {{{
 *   @inlineFormat
 *   case class Wrapper(x: OtherObject)
 * }}}
 * `OtherObject` will be actually serialized, no wrapper class will be seen.
 */
class inlineFormat extends StaticAnnotation
