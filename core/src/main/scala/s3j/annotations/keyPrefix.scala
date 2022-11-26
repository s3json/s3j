package s3j.annotations

import scala.annotation.Annotation

/**
 * When used with `@inlineObject`, causes keys of inline fields to be prefixed with given prefix. Empty prefix is a 
 * special value and causes fields' own key to be used as a prefix.
 * 
 * @param prefix Prefix for keys in inline objects
 */
class keyPrefix(prefix: String = "") extends Annotation
