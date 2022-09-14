package s3j.annotations.naming

import scala.annotation.StaticAnnotation

/**
 * Defines default field case, which will be applied unless overridden with specific annotation
 */
class defaultFieldCase(c: CaseConvention) extends StaticAnnotation
