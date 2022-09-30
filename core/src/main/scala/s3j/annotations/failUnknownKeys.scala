package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When applied to a case class, causes parser to raise an error upon seeing unknown field instead of dropping it.
 *
 * Undoes effect of [[allowUnknownKeys]].
 */
class failUnknownKeys extends StaticAnnotation
