package s3j.schema

import s3j.schema.InlineSchema.Dependencies
import s3j.schema.impl.InlineSchemaMacros

object InlineSchema {
  /** Supertype for dependent schema specification */
  type Dependencies = Tuple | Unit | JsonSchema[?]

  /**
   * Automatically derives matching value for `InlineSchema` type. For example, this definition
   *
   * {{{
   *   given stringSchema: InlineSchema[String, "{\"type\":\"string\"}", Unit] = InlineSchema.make
   * }}}
   *
   * expands to:
   *
   * {{{
   *   given stringSchema: InlineSchema[String, "{\"type\":\"string\"}", Unit] =
   *    new JsonSchema("{\"type\":\"string\"}", shouldInline = true)
   * }}}
   *
   * Dependent schemas are specified as a tupled type in `D` type argument. This tupled type should consist of singleton
   * types with references to dependent schemas. JSON string should reference these schemas with `{"$ref":"~NNN"}`
   * property (in the same way as runtime schema references works). For example,
   *
   * {{{
   *   type SchemaJson = """{"type":"object","propertyNames":{"$ref":"~0"},"additionalProperties":{"$ref":"~1"}}"""
   *   given mapSchema[K, V](using k: JsonSchema[K], v: JsonSchema[V]): InlineSchema[Map[K, V], SchemaJson, (k.type, v.type)] =
   *    InlineSchema.make
   * }}}
   *
   * will expand to:
   *
   * {{{
   *   type SchemaJson = """{"type":"object","propertyNames":{"$ref":"~0"},"additionalProperties":{"$ref":"~1"}}"""
   *   given mapSchema[K, V](using k: JsonSchema[K], v: JsonSchema[V]): InlineSchema[Map[K, V], SchemaJson, (k.type, v.type)] =
   *    new InlineSchema(/* json string skipped */, Vector(k, v), shouldInline = true)
   * }}}
   *
   * Compiler will walk inline schemas recursively (even though dependent schemas are specified as `JsonSchema[?]`,
   * not as `InlineSchema[?, ?, ?]`) and combine everything it finds into big JSON string. Non-inline dependent schemas
   * will be left as runtime references.
   */
  inline def make[T, S <: String & Singleton, D <: Dependencies]: InlineSchema[T, S, D] =
    ${ InlineSchemaMacros.generateInline[T] }
}

/**
 * Wrapper type for schemas that could be inlined at compile time.
 *
 * @tparam T Type of object representing this schema
 * @tparam S JSON string with schema contents to be inlined
 * @tparam D Dependent schemas, referenced in the same way as runtime dependencies in [[JsonSchema]] class.
 */
type InlineSchema[T, S <: String & Singleton, D <: Dependencies] = JsonSchema[T]
