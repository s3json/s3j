package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.*
import s3j.core.casecls.{CaseClassContext, CaseClassExtension, CaseClassUtils}
import s3j.core.casecls.impl.CaseClassObjectBuilder.{FieldIdentity, ObjectIdentity, StackEntry}
import s3j.core.casecls.modifiers.{FieldCaseModifier, FieldKeyModifier, KeyPrefixModifier, UnknownKeysModifier}
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.GenerationContext
import s3j.macros.PluginContext.ExtensionRegistration
import s3j.macros.codegen.{CodeUtils, Variable, Position as XPosition}
import s3j.macros.generic.CaseConvention
import s3j.macros.modifiers.ModifierSet
import s3j.macros.schema.SchemaExpr
import s3j.macros.schema.modifiers.{SchemaDeprecatedModifier, SchemaDescriptionModifier, SchemaHiddenModifier, SchemaTitleModifier}
import s3j.macros.traits.{ErrorReporting, ReportingBuilder}
import s3j.macros.utils.{GenerationPath, ReportingUtils}
import s3j.schema.model.{SchemaAnnotations, SchemaDocument}

import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.quoted.runtime.StopMacroExpansion
import scala.util.control.NonFatal

private[casecls] object CaseClassObjectBuilder {
  case class StackEntry(t: Type[?], modifiers: ModifierSet, path: Seq[GenerationPath], label: Option[String]) {
    def toReporting: ReportingBuilder.StackEntry =
      ReportingBuilder.StackEntry(t, label, path)
  }

  case class FieldIdentity(handledKeys: Set[String], dynamic: Boolean, field: AnyRef)
  case class ObjectIdentity(unknownKeys: Boolean, fields: Set[FieldIdentity])
}

private[casecls] class CaseClassObjectBuilder[R](stack: List[StackEntry])(using c: GenerationContext, q: Quotes)
{ outer =>
  import q.reflect.*

  private val generatedType: TypeRepr = TypeRepr.of(using stack.head.t)
  private val typeArgs: List[TypeRepr] = generatedType.typeArgs
  private val typeSymbol: Symbol = generatedType.typeSymbol
  private val typeParams: List[Symbol] = typeSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam)
  private val modifiers: ModifierSet = stack.head.modifiers
  private val reportingStackBase: Seq[ReportingBuilder.StackEntry] = outer.stack.map(_.toReporting).reverse.tail

  given Type[R] = stack.head.t.asInstanceOf[Type[R]]

  if (typeArgs.size != typeParams.size) {
    throw new IllegalArgumentException("Type parameters and arguments list differ in length for: " +
      typeSymbol.fullName + " (" + generatedType.show + ")")
  }

  private val report: ErrorReporting = c.reportBuilder
    .stackEntries(reportingStackBase:_*)
    .build()

  private val keyPrefix: Option[String] = outer.modifiers
    .get(KeyPrefixModifier).map(_.prefix)
    .filter(_.nonEmpty)

  private class FieldImpl(val ctorField: Symbol, val listIndex: Int) {
    val classField: Symbol = typeSymbol.fieldMember(ctorField.name)
    val fieldType: TypeRepr = generatedType.memberType(ctorField).substituteTypes(typeParams, typeArgs).simplified.dealias
    val ownModifiers: ModifierSet = c.symbolModifiers(ctorField).own
    val inheritedModifiers: ModifierSet = ModifierSet.inherit(outer.modifiers, ownModifiers)

    /**
     * N.B.: This key is only an initial suggested key for the field! Extensions which are responsible for actual code
     * generation may either use this key as-is, modify it in some way, or completely ignore it!
     *
     * Use [[result.handledKeys]] and [[result.handlesDynamicKeys]] to query actual keys for this field.
     */
    val baseKey: String = {
      val caseConvention = inheritedModifiers(FieldCaseModifier).value
      val ownKey = ownModifiers
        .get(FieldKeyModifier).map(_.fieldKey)
        .getOrElse(caseConvention.transform(ctorField.name))

      keyPrefix match {
        case Some(prefix) => caseConvention.concat(prefix, ownKey)
        case None => ownKey
      }
    }

    val reportPosition: Option[XPosition] = ctorField.pos.map(XPosition.apply(_))
    val reportingStack: Seq[ReportingBuilder.StackEntry] = reportingStackBase :+
      ReportingBuilder.StackEntry(fieldType.asType, None, Seq(GenerationPath.ObjectField(ctorField.name)))

    type T
    val request: FieldRequest[T] =
      FieldRequest[T](ctorField.name, baseKey, fieldType.asType.asInstanceOf[Type[T]], ownModifiers, stack.head.modifiers)

    @threadUnsafe
    lazy val report: ErrorReporting = c.reportBuilder
      .stackEntries(reportingStack:_*)
      .defaultPosition(reportPosition)
      .build()

    var result: ObjectField[T] = _

    override def toString: String =
      s"{field='${ctorField.name}', type='${fieldType.show}', modifiers=$inheritedModifiers, listIdx=$listIndex}"
  }

  // Scan all fields to have plugins loaded before getting extensions
  private val fields: Seq[FieldImpl] = typeSymbol.primaryConstructor.paramSymss
    .filterNot(_.forall(_.isTypeParam))
    .zipWithIndex.flatMap { case (xs, i) => xs.filterNot(_.isTypeParam).map(new FieldImpl(_, i)) }
    .toVector

  private val extensions: Set[ExtensionRegistration[CaseClassExtension]] = c.extensions(CaseClassExtension)

  private class FieldContextImpl(val f: FieldImpl, val ext: ExtensionRegistration[CaseClassExtension])
  extends CaseClassContext {
    // TODO: Wrap 'nested' with added generation path
    export c.{generationMode, symbolModifiers, loadPlugin, plugins, extensions, nested, usePlugin}

    val report: ErrorReporting = c.reportBuilder
      .usePlugin(ext.pluginClass)
      .defaultPosition(f.reportPosition)
      .stackEntries(f.reportingStack:_*)
      .build()

    val outcome: GenerationOutcome[f.T] =
      try ext.instance.processField(using this)(f.request)(using outer.q, f.request.fieldType)
      catch {
        case e: StopMacroExpansion => throw e
        case NonFatal(e) => report.errorAndAbort(ReportingUtils.formatException("Plugin failed to produce case class " +
          "generation outcome", e))
      }

    val candidate: Option[GenerationCandidate[f.T]] =
      outcome match { case c: GenerationCandidate[f.T @unchecked] => Some(c) case _ => None }

    override def toString: String = s"FieldContext(field=$f, outcome=$outcome)"

    def objectModel[T](using t: Type[T]): ObjectModelBuilder[T] = new ObjectModelBuilder[T] {
      private val _typeRepr: TypeRepr = TypeRepr.of[T].simplified.dealias
      private val _typeSymbol: Symbol = _typeRepr.typeSymbol
      private var _modifiers: ModifierSet =
        ModifierSet.inherit(stack.head.modifiers, f.ownModifiers, c.symbolModifiers(_typeSymbol).inherited)

      private var _path: Seq[GenerationPath] = Vector.empty
      private var _label: Option[String] = None

      if (!_typeSymbol.isClassDef || !_typeSymbol.flags.is(Flags.Case)) {
        throw new IllegalArgumentException("Cannot generate nested model for non-case class: " + Type.show[T])
      }

      def discardModifiers(): this.type = {
        _modifiers = ModifierSet.empty
        this
      }

      def modifiers(mods: ModifierSet): this.type = {
        _modifiers = _modifiers ++ mods
        this
      }

      def generationPath(p: GenerationPath*): this.type = {
        _path = _path ++ p
        this
      }

      def label(s: String): this.type = {
        _label = Some(s)
        this
      }

      def build(): ObjectModel[T] = {
        val nextStack = StackEntry(t, _modifiers, _path, _label) :: stack
        checkStack(nextStack)
        new CaseClassObjectBuilder(nextStack).result.asInstanceOf[ObjectModel[T]]
      }
    }
  }

  private def checkStack(stack: List[StackEntry]): Unit = {
    // TODO: Check for recursion
  }

  private def populateField(field: FieldImpl): Unit = {
    val contexts = extensions.map(new FieldContextImpl(field, _))
    if (contexts.forall(_.candidate.isEmpty)) {
      field.report.errorAndAbort("No plugin is available to handle generation of this field")
    }

    val certainCount = contexts.count(_.candidate.exists(_.confidence.isCertain))
    if (certainCount > 1) {
      val sb = new mutable.StringBuilder()
      sb ++= "Multiple plugins produced conflicting field candidates:\n"

      for (c <- contexts if c.candidate.exists(_.confidence.isCertain)) {
        sb ++= " - " ++= c.ext.pluginInstance.name ++= " (plugin '" ++= c.ext.pluginClass ++= "', extension '"
        sb ++= c.ext.instance.getClass.getName ++= "')\n"
      }

      field.report.errorAndAbort(sb.result())
    }

    val result: FieldContextImpl =
      if (certainCount > 0) contexts.find(_.candidate.exists(_.confidence.isCertain)).get
      else contexts.filter(_.candidate.isDefined).maxBy(_.candidate.get.confidence.value)

    field.result =
      try result.candidate.get.result.asInstanceOf[ObjectField[field.T]]
      catch {
        case e: StopMacroExpansion => throw e
        case NonFatal(e) => report.errorAndAbort(ReportingUtils.formatException("Plugin failed to produce case class " +
          "field for positive candidate", e))
      }
  }

  private def checkConsistency(): Unit = {
    for ((k, vs) <- fields.flatMap(f => f.result.handledKeys.map(_ -> f)).groupMap(_._1)(_._2) if vs.size > 1) {
      report.errorAndAbort(s"Multiple fields have conflicting key '$k': ${vs.map(_.ctorField.name).mkString(", ")}")
    }

    if (fields.count(_.result.handlesDynamicKeys) > 1) {
      report.errorAndAbort(s"Multiple fields have dynamic field keys: " +
        fields.filter(_.result.handlesDynamicKeys).map(_.ctorField.name).mkString(", "))
    }
  }

  private def computeIdentity(): ObjectIdentity =
    ObjectIdentity(
      unknownKeys = stack.head.modifiers(UnknownKeysModifier).allow,
      fields = fields
        .map { f => FieldIdentity(f.result.handledKeys, f.result.handlesDynamicKeys, f.result.identity) }
        .toSet
    )

  // Restore previously flattened field list to List[List[X]]
  private def groupFields[T, R](fields: Seq[T])(listIndex: T => Int, result: T => R): List[List[R]] = {
    val r = List.newBuilder[List[R]]
    var rr = List.newBuilder[R]
    var lastIndex = 0

    for (f <- fields) {
      val idx = listIndex(f)
      if (idx < lastIndex || idx > lastIndex + 1) {
        throw new IllegalArgumentException("listIndex is not monotonic: " + fields.map(listIndex).mkString(", "))
      }

      if (idx > lastIndex) {
        r += rr.result()
        rr = List.newBuilder[R]
      }

      rr += result(f)
      lastIndex = idx
    }

    r += rr.result()
    r.result()
  }

  private def augmentSchema[T](f: FieldImpl, s: SchemaExpr[T]): SchemaExpr[T] =
    SchemaExpr.augmentModifiers(s, f.inheritedModifiers)

  private def ownRootSchema: SchemaExpr.Inlined[R] =
    SchemaExpr.augmentModifiers(SchemaExpr.Inlined(
      document = SchemaDocument(
        annotations = SchemaAnnotations(
          title = Some(typeSymbol.name)
        )
      ),
      shouldInline = true
    ), modifiers).asInlined

  private class NestedCode[N](f: FieldImpl => N, fieldFilter: FieldImpl => Boolean = _ => true) {
    val fields: Seq[FieldImpl] = outer.fields.filter(fieldFilter)
    val code: Seq[N] = fields.map(f)
    val keyFields: Map[String, FieldImpl] = fields.flatMap(f => f.result.handledKeys.map(_ -> f)).toMap
    val keys: Map[String, N] = fields.zip(code).flatMap { case (f, c) => f.result.handledKeys.map(_ -> c) }.toMap
    val dynamicKeyField: Option[FieldImpl] = fields.find(_.result.handlesDynamicKeys)
    val dynamicKeys: Option[N] = fields.zip(code).find(_._1.result.handlesDynamicKeys).map(_._2)

    def key(s: String): N =
      keys.getOrElse(s, throw new IllegalArgumentException(s"Key '$key' is not present in the object"))

    def dynamicKey: N =
      dynamicKeys.getOrElse(throw new IllegalArgumentException("Dynamic keys are not present in the object"))
  }

  val result: ObjectModel[R] = {
    for (f <- fields) populateField(f)
    checkConsistency()

    new ObjectModel[R] {
      val identity: AnyRef = computeIdentity()
      val fields: Seq[ObjectField[_]] = outer.fields.map(_.result)
      val dynamicField: Option[ObjectField[_]] = fields.find(_.handlesDynamicKeys)
      val handledKeys: Set[String] = fields.flatMap(_.handledKeys).toSet
      val handlesDynamicKeys: Boolean = dynamicField.isDefined

      def generateEncoder(writer: Expr[JsonWriter], value: Expr[R])
                         (using Quotes, GenerationEnvironment): Expr[Any] =
        CodeUtils.joinBlocks(outer.fields.map { f =>
          import quotes.reflect.*
          import f.request.implicitType

          val selectedField = Select(value.asTerm, f.classField.asInstanceOf[Symbol]).asExprOf[f.T]
          f.result.generateEncoder(writer, selectedField)
        })

      def generateDecoder(using Quotes, DecodingEnvironment): DecodingCode =
        new DecodingCode {
          private val c: NestedCode[DecodingCode] = new NestedCode(_.result.generateDecoder)

          def usedVariables: Seq[Variable[_]] = c.code.flatMap(_.usedVariables)

          def decodeKey(key: String, reader: Expr[JsonReader])(using Quotes): Expr[Any] =
            c.key(key).decodeKey(key, reader)

          override def decodeDynamicKey(key: Expr[String], reader: Expr[JsonReader])(using Quotes): Expr[Any] =
            c.dynamicKey.decodeDynamicKey(key, reader)

          def decodeFinal()(using Quotes): Expr[Any] =
            CodeUtils.joinBlocks(c.code.map(_.decodeFinal()))

          def decodeResult()(using Quotes): Expr[Any] = {
            given Type[R] = stack.head.t.asInstanceOf[Type[R]]
            CaseClassUtils.createInstance[R](c.code.map(_.decodeResult()), typeArgs.map(_.asType))
          }
        }

      /** @return Generated schemas for the field */
      def generateSchema(using Quotes): SchemaCode[R] =
        new SchemaCode {
          private val c: NestedCode[SchemaCode[?]] = new NestedCode(_.result.generateSchema,
            f => !f.inheritedModifiers.contains(SchemaHiddenModifier))

          def key(key: String): SchemaExpr[?] = {
            val code = c.key(key)
            if (code.suppressAugmentation) code.key(key)
            else augmentSchema(c.keyFields(key), code.key(key))
          }

          def dynamicKey: Option[SchemaExpr[?]] = {
            val code = c.dynamicKey
            if (code.suppressAugmentation) code.dynamicKey
            else code.dynamicKey.map(augmentSchema(c.dynamicKeyField.get, _))
          }

          def keyOrdering: Seq[String] = c.code.flatMap(_.keyOrdering)
          def requiredKeys: Set[String] = c.code.flatMap(_.requiredKeys).toSet
          def dynamicKeyNames: Option[SchemaExpr[String]] = c.dynamicKey.dynamicKeyNames
          override def rootSchema: Option[SchemaExpr.Inlined[R]] = Some(ownRootSchema)
        }

      override def toString: String = s"ObjectField(fields=[${fields.mkString(", ")}])"
    }
  }
}
