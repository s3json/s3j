package s3j.annotations

import scala.annotation.StaticAnnotation

/** 
 * When applied to a case class, causes parser to silently drop unknown fields instead of raising an error.
 * 
 * Undoes effect of [[failUnknownKeys]].
 */
class allowUnknownKeys() extends StaticAnnotation
