package s3j.annotations

import scala.annotation.StaticAnnotation

/** Specifies a value to use when selecting a class from sealed hierarchy (i.e. 'name of this class'). */
case class discriminator(key: String) extends StaticAnnotation
