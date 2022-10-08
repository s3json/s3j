package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When enum has only singleton cases, causes it to be serialized as full object anyway. By default enums with all-
 * singleton cases are serialized as simple strings.
 */
class objectEnum extends StaticAnnotation
