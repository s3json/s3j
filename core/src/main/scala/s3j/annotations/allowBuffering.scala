package s3j.annotations

import scala.annotation.StaticAnnotation

/** 
 * When applied to an enum or sealed class, allows discriminator field not to come first, buffering all input until
 * discriminator field is reached.
 */
class allowBuffering() extends StaticAnnotation
