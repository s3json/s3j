package s3j.annotations.naming

import scala.annotation.StaticAnnotation

/**
 * Forces macros to transform field names to kebab case (e.g. `someField` -> `some-field`). Works on enumerations and
 * transforms enumeration constant names.
 */
class kebabCase extends StaticAnnotation
