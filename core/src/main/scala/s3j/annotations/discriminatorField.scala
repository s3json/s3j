package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * Overrides name of the discriminator field.
 */
case class discriminatorField(name: String) extends StaticAnnotation
