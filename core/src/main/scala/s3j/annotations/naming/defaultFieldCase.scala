package s3j.annotations.naming

import s3j.macros.generic.CaseConvention
import scala.annotation.StaticAnnotation

/**
 * Defines default field case, which will be applied unless overridden with specific annotation
 */
class defaultFieldCase(cc: CaseConvention | String) extends StaticAnnotation
