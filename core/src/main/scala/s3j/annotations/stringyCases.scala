package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When enum has mixed singleton and class cases, causes singletons to be serialized as strings, just as for enums with
 * all singletons. Default behavior is to always switch to full-object serialization when at least one non-value case is
 * present.
 */
class stringyCases extends StaticAnnotation
