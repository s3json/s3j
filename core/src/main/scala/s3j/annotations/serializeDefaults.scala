package s3j.annotations

import scala.annotation.StaticAnnotation

/**
 * When applied to a field, causes serializer to emit default value even (by default only differing values are emitted).
 * When applied to class, applies that behavior to all fields in class.
 */
class serializeDefaults extends StaticAnnotation
