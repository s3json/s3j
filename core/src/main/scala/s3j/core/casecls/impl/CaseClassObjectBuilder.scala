package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.*
import s3j.core.casecls.{CaseClassContext, CaseClassExtension}
import s3j.core.casecls.impl.CaseClassObjectBuilder.{FieldIdentity, ObjectIdentity, StackEntry}
import s3j.core.casecls.modifiers.UnknownKeysModifier
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.GenerationContext
import s3j.macros.PluginContext.ExtensionRegistration
import s3j.macros.codegen.{CodeUtils, Variable, Position as XPosition}
import s3j.macros.modifiers.ModifierSet
import s3j.macros.traits.{ErrorReporting, ReportingBuilder}
import s3j.macros.utils.{GenerationPath, ReportingUtils}

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

  if (typeArgs.size != typeParams.size) {
    throw new IllegalArgumentException("Type parameters and arguments list differ in length for: " +
      typeSymbol.fullName + " (" + generatedType.show + ")")
  }

  private val report: ErrorReporting = c.reportBuilder
    .stackEntries(reportingStackBase:_*)
    .build()

  private class FieldImpl(val ctorField: Symbol, val listIndex: Int) {
    val classField: Symbol = typeSymbol.fieldMember(ctorField.name)
    val fieldType: TypeRepr = generatedType.memberType(ctorField).substituteTypes(typeParams, typeArgs).simplified.dealias
    val ownModifiers: ModifierSet = c.symbolModifiers(ctorField).own
    val inheritedModifiers: ModifierSet = outer.modifiers ++ ownModifiers

    val key: String = ctorField.name // TODO: Case conventions, key overrides

    val reportPosition: Option[XPosition] = ctorField.pos.map(XPosition.apply(_))
    val reportingStack: Seq[ReportingBuilder.StackEntry] = reportingStackBase :+
      ReportingBuilder.StackEntry(fieldType.asType, None, Seq(GenerationPath.ObjectField(ctorField.name)))

    type T
    val request: FieldRequest[T] =
      FieldRequest[T](ctorField.name, key, fieldType.asType.asInstanceOf[Type[T]], ownModifiers, stack.head.modifiers)

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

  private val extensions: Set[ExtensionRegistration[CaseClassExtension]] = c.extensions(CaseClassExtension.key)

  private class FieldContextImpl(val f: FieldImpl, val ext: ExtensionRegistration[CaseClassExtension])
  extends CaseClassContext {
    // TODO: Wrap 'nested' with added generation path
    export c.{generationMode, freshName, symbolModifiers, loadPlugin, plugins, extensions, nested}

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
        stack.head.modifiers ++ c.symbolModifiers(_typeSymbol).inherited ++ f.ownModifiers

      private var _path: Seq[GenerationPath] = Vector.empty
      private var _label: Option[String] = None

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

    val confidentCount = contexts.count(_.candidate.exists(_.confidence.isEmpty))
    if (confidentCount > 1) {
      val sb = new mutable.StringBuilder()
      sb ++= "Multiple plugins produced conflicting field candidates:\n"

      for (c <- contexts if c.candidate.exists(_.confidence.isEmpty)) {
        sb ++= " - " ++= c.ext.pluginInstance.name ++= " (plugin '" ++= c.ext.pluginClass ++= "', extension '"
        sb ++= c.ext.instance.getClass.getName ++= "')\n"
      }

      field.report.errorAndAbort(sb.result())
    }

    val result: FieldContextImpl =
      if (confidentCount > 0) contexts.find(_.candidate.exists(_.confidence.isEmpty)).get
      else contexts.filter(_.candidate.isDefined).maxBy(_.candidate.get.confidence.get)

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
      unknownKeys = stack.head.modifiers(UnknownKeysModifier.key).allow,
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
          private val fieldCode: Seq[DecodingCode] = outer.fields.map(_.result.generateDecoder)
          private val keyCode: Map[String, DecodingCode] =
            outer.fields.zip(fieldCode)
              .flatMap { case (f, c) => f.result.handledKeys.map(_ -> c) }
              .toMap

          private val dynamicKeyCode: Option[DecodingCode] =
            outer.fields.zip(fieldCode)
              .find(_._1.result.handlesDynamicKeys)
              .map(_._2)

          def usedVariables: Seq[Variable[_]] = fieldCode.flatMap(_.usedVariables)

          def decodeKey(key: String, reader: Expr[JsonReader])(using Quotes): Expr[Any] =
            keyCode
              .getOrElse(key, throw new IllegalArgumentException(s"Key '$key' is not present in the object"))
              .decodeKey(key, reader)

          override def decodeDynamicKey(key: Expr[String], reader: Expr[JsonReader])(using Quotes): Expr[Any] =
            dynamicKeyCode
              .getOrElse(throw new IllegalArgumentException("Dynamic keys are not present in the object"))
              .decodeDynamicKey(key, reader)

          def decodeFinal()(using Quotes): Expr[Any] =
            CodeUtils.joinBlocks(fieldCode.map(_.decodeFinal()))

          def decodeResult()(using Quotes): Expr[Any] = {
            import quotes.reflect.*
            var resultType: TypeTree = TypeIdent(typeSymbol.asInstanceOf[Symbol])

            if (typeParams.nonEmpty) {
              resultType = Applied(resultType, typeArgs.map(t => Inferred(t.asInstanceOf[TypeRepr])))
            }

            var result: Term = Select(New(resultType), typeSymbol.primaryConstructor.asInstanceOf[Symbol])
            if (typeParams.nonEmpty) {
              result = TypeApply(result, typeArgs.map(t => Inferred(t.asInstanceOf[TypeRepr])))
            }

            groupFields(outer.fields.zip(fieldCode))(_._1.listIndex, _._2.decodeResult())
              .foldLeft(result)((t, args) => Apply(t, args.map(_.asTerm)))
              .asExpr
          }
        }
    }
  }
}
