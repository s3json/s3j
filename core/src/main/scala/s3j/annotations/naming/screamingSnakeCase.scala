package s3j.annotations.naming

import scala.annotation.StaticAnnotation

/**
 * Forces macros to transform field names to screaming snake case (e.g. `someField` -> `SOME_FIELD`).
 * Works on enumerations and transforms enumeration constant names.
 */
class screamingSnakeCase extends StaticAnnotation
