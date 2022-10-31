package s3j.core.casecls.impl

import s3j.macros.schema.{SchemaExpr, SchemaExprBuilder}
import s3j.core.casecls.CaseClassContext.{ObjectField, SchemaCode}
import s3j.macros.modifiers.ModifierSet
import s3j.schema.model.{SchemaOrFalse, ObjectSchema, OrderedMap, SchemaDocument, SchemaType}

import scala.quoted.{Quotes, Type}

private[casecls] class CaseClassSchemaGenerator[T](field: ObjectField[T])(using Quotes, Type[T]) {
  private val schema: SchemaCode[T] = field.generateSchema

  private val properties: Seq[(String, SchemaExpr[?])] = schema.keyOrdering.map(k => k -> schema.key(k))

  private val resultRaw: SchemaExpr[T] =
    SchemaExpr.build() { b =>
      SchemaDocument(
        `type` = Some(SchemaType.Object),
        `object` = ObjectSchema(
          properties = Some(properties)
            .filter(_.nonEmpty)
            .map(_.map((k, v) => k -> b.reference(v)))
            .map(r => OrderedMap(r:_*)),
          required = Some(schema.requiredKeys).filter(_.nonEmpty),
          additionalProperties =
            if (!field.handlesDynamicKeys) Some(SchemaOrFalse.Forbidden)
            else schema.dynamicKey.map(s => SchemaOrFalse.Schema(b.reference(s))),
          propertyNames =
            if (!field.handlesDynamicKeys) None
            else schema.dynamicKeyNames.map(b.reference)
        )
      )
    }

  val result: SchemaExpr[T] = schema.rootSchema.fold(resultRaw)(SchemaExpr.overlay(resultRaw, _))
}
