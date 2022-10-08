package s3j.annotations.naming

import scala.annotation.StaticAnnotation

/**
 * Forces macros to transform field names to capitalized kebab case (e.g. `someField` -> `Some-Field`).
 * Works on enumerations and transforms enumeration constant names.
 */
class capitalizedKebabCase extends StaticAnnotation
