package s3j.annotations.naming

import scala.annotation.StaticAnnotation

/**
 * Forces macros to transform field names to pascal case (e.g. `someField` -> `SomeField`). Works on enumerations and
 * transforms enumeration constant names.
 */
class pascalCase extends StaticAnnotation
