package s3j.annotations

import s3j.macros.Plugin

import scala.annotation.StaticAnnotation

/**
 * Upon seeing that annotation on type or other annotation, macro engine will instantiate specified plugin and use it
 * to serialize type or parse annotation.
 *
 * This acts as a platform-independent way to discover used plugins.
 *
 * @tparam T Plugin type
 */
class usePlugin[T <: Plugin] extends StaticAnnotation
