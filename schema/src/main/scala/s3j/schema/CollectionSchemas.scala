package s3j.schema

object CollectionSchemas {
  type SeqSchemaJson = """{"type":"array","items":{"$ref":"~0"}}"""
  given seqSchema[T, C[X] <: Iterable[X]](using s: JsonSchema[T]): InlineSchema[C[T], SeqSchemaJson, s.type] =
    InlineSchema.make

  type SetSchemaJson = """{"type":"array","uniqueItems":true,"items":{"$ref":"~0"}}"""
  given setSchema[T, C[X] <: Set[X]](using s: JsonSchema[T]): InlineSchema[C[T], SetSchemaJson, s.type] =
    InlineSchema.make

  type MapSchemaJson = """{"type":"object","propertyNames":{"$ref":"~0"},"additionalProperties":{"$ref":"~1"}}"""
  given mapSchema[K, V](using k: JsonSchema[K], v: JsonSchema[V]):
    InlineSchema[Map[K, V], MapSchemaJson, (k.type, v.type)] = InlineSchema.make

  // Omit propertyNames for trivial cases
  type StringMapSchemaJson = """{"type":"object","additionalProperties":{"$ref":"~0"}}"""
  given mapStringSchema[V](using v: JsonSchema[V]): InlineSchema[Map[String, V], StringMapSchemaJson, v.type] =
    InlineSchema.make
}
