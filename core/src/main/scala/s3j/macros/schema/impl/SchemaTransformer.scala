package s3j.macros.schema.impl

import s3j.ast.JsValue
import s3j.macros.modifiers.ModifierSet
import s3j.macros.schema.SchemaExpr
import s3j.macros.schema.modifiers.{SchemaDeprecatedModifier, SchemaDescriptionModifier, SchemaTitleModifier}
import s3j.schema.model.{CompositionSchema, InternalReference, S3jAnnotations, SchemaAnnotations, SchemaDocument, SchemaType}

import scala.quoted.{Quotes, Type}

private[schema] object SchemaTransformer {
  private val schemaModifiers = Seq(
    SchemaTitleModifier, SchemaDescriptionModifier, SchemaDeprecatedModifier
  )

  def augmentModifiers[T](sch: SchemaExpr[T], mods: ModifierSet)(using Quotes): SchemaExpr[T] = {
    if (!mods.containsAny(schemaModifiers: _*)) return sch

    SchemaExpr.inlineParts(SchemaExpr.Inlined(
      document = SchemaDocument(
        `$ref` = Some(InternalReference(0, forceInline = true).toString),
        annotations = SchemaAnnotations(
          title = mods.get(SchemaTitleModifier).map(_.title),
          description = mods.get(SchemaDescriptionModifier).map(_.description),
          deprecated = mods.get(SchemaDeprecatedModifier).map(_ => true)
        )
      ),
      definitions = Vector(sch),
      shouldInline = sch.canInline
    ))
  }

  def overlay[T](root: SchemaExpr[T], overlay: SchemaExpr.Inlined[T]): SchemaExpr[T] = {
    val rootInl: Option[SchemaExpr.Inlined[T]] = root match {
      case inl: SchemaExpr.Inlined[T] => Some(inl)
      case _ => None
    }

    val rootRef = rootInl.fold(root)(_.copy(defaultValue = None, exampleValues = None))

    val rootIndex = overlay.definitions.size
    SchemaExpr.inlineParts[T](SchemaExpr.Inlined(
      document = overlay.document.copy(`$ref` = Some(InternalReference(rootIndex, forceInline = true).toString)),
      definitions = overlay.definitions :+ rootRef,
      shouldInline = root.shouldInline,
      defaultValue = overlay.defaultValue.orElse(rootInl.flatMap(_.defaultValue)),
      exampleValues = overlay.exampleValues.orElse(rootInl.flatMap(_.exampleValues))
    ))
  }

  def makeOptional[T](root: SchemaExpr[T]): SchemaExpr[Option[T]] =
    root match {
      case root: SchemaExpr.Inlined[T] if root.document.`type`.isDefined =>
        // just append "null" to "type" and it's done
        root.copy(
          document = root.document.copy(`type` = Some(root.document.`type`.get :+ SchemaType.Null))
        )

      // Fallback to anyOf mechanics:
      case _ => SchemaExpr.build(shouldInline = true) { b =>
        SchemaDocument(
          composition = CompositionSchema(
            anyOf = Some(Seq(
              SchemaDocument(`type` = Some(SchemaType.Null)),
              b.reference(root)
            ))
          )
        )
      }
    }
}
