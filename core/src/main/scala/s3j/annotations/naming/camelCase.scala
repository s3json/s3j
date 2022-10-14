package s3j.annotations.naming

import scala.annotation.StaticAnnotation

/**
 * Forces macros to transform field names to camel case (e.g. `someField`). Works on enumerations and
 * transforms enumeration constant names.
 */
class camelCase extends StaticAnnotation
