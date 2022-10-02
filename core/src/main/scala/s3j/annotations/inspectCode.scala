package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * Annotation, when present on root generated type, causes macro engine to abort and dump generated code. Not inherited
 * like other modifiers: must be placed on root type directly.
 */
class inspectCode extends StaticAnnotation
