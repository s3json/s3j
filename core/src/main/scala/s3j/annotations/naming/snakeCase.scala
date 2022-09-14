package s3j.annotations.naming

import scala.annotation.StaticAnnotation

/**
 * Forces macros to transform field names to snake case (e.g. `someField` -> `some_field`). Works on enumerations and
 * transforms enumeration constant names.
 */
class snakeCase extends StaticAnnotation
