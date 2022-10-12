package s3j.macros.generic

import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.format.util.Recursive
import s3j.macros.GenerationContext.{GenerationCandidate, GenerationOutcome, GenerationRejection}
import s3j.macros.codegen.{AssistedImplicits, Position as XPosition}
import s3j.macros.{GenerationContext, PluginContext, PluginCapability}
import s3j.macros.modifiers.{BuiltinModifiers, ModifierSet}
import s3j.macros.traits.{ErrorReporting, NestedBuilder, NestedResult, ReportingBuilder}
import s3j.macros.utils.{ForbiddenMacroUtils, GenerationPath, MacroUtils, ReportingUtils}

import java.io.{PrintWriter, StringWriter}
import scala.util.control.NonFatal
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}
import scala.quoted.runtime.StopMacroExpansion
import scala.quoted.runtime.impl.QuotesImpl

private[macros] trait GenerationContextImpl { outer: PluginContextImpl[_] =>
  import q.reflect.*
  import q.{reflect => or}

  private var _serializerIndex: Int = 0

  private case object ImplicitIdentity
  case class StackEntry(target: TypeRepr, modifiers: ModifierSet, path: Seq[GenerationPath], label: Option[String],
                        mode: GenerationMode)

  case class GenerationTask(stack: List[StackEntry]) {
    type T

    val isRoot: Boolean = stack.tail.isEmpty
    val target: TypeRepr = stack.head.target
    val targetType: Type[T] = target.asType.asInstanceOf[Type[T]]
    val typeSymbol: Symbol = target.typeSymbol
    val modifiers: ModifierSet = stack.head.modifiers
    val mode: GenerationMode = stack.head.mode

    val basicKey: BasicCachingKey = BasicCachingKey(target, modifiers)
    val implicitKey: ImplicitCachingKey = ImplicitCachingKey(target)
  }

  private class NestedResultImpl[T](handle: SerializerHandle)(using Type[T]) extends NestedResult[T] {
    def raw(using q: Quotes): Expr[Any] = {
      import q.reflect.*
      Ref(handle.variableSymbol.asInstanceOf[Symbol]).asExpr
    }

    def encoder(using Quotes): Expr[JsonEncoder[T]] = {
      if (!generationMode.generateEncoders) {
        throw new IllegalStateException("Encoders are not generated in mode " + generationMode)
      }

      raw.asExprOf[JsonEncoder[T]]
    }

    def decoder(using Quotes): Expr[JsonDecoder[T]] = {
      if (!generationMode.generateDecoders) {
        throw new IllegalStateException("Decoders are not generated in mode " + generationMode)
      }

      raw.asExprOf[JsonDecoder[T]]
    }
  }

  private abstract class BasicContextImpl(val task: GenerationTask, val owner: PluginContainer)
  extends GenerationContext { inner =>
    protected def generating: Boolean

    val generationMode: GenerationMode = task.mode
    val generatedType: Type[T] = task.target.asType.asInstanceOf[Type[T]]
    val typeSymbol: Symbol = task.target.typeSymbol
    val modifiers: ModifierSet = task.modifiers

    val report: ErrorReporting = outer.report.transform(
      msgTransformer = Some(formatMessage(_, task, Some(owner)))
    )

    protected def reportException(msg: String, e: Throwable): Nothing =
      report.errorAndAbort(ReportingUtils.formatException(msg, e))

    /** @return New builder for nested serializer */
    def nested(using q: Quotes)(t: q.reflect.TypeRepr): NestedBuilder[?] = {
      type T
      nested[T](using t.asType.asInstanceOf[Type[T]])
    }

    def nested[T](using tpe: Type[T]): NestedBuilder[T] = {
      if (!generating) {
        throw new IllegalStateException("nested() generation must not be called before actual generation starts")
      }

      val typeSymbol = TypeRepr.of[T].typeSymbol
      new NestedBuilder {
        var _modifiers: ModifierSet = outer.symbolModifiers(typeSymbol).inherited
        var _genPath: Vector[GenerationPath] = Vector.empty

        def discardModifiers(): this.type = {
          _modifiers = ModifierSet.empty
          this
        }

        def modifiers(modifiers: ModifierSet): this.type = {
          _modifiers = _modifiers ++ modifiers
          this
        }

        def generationPath(path: GenerationPath*): this.type = {
          _genPath = _genPath ++ path
          this
        }

        def build(): NestedResult[T] = {
          val raw = outer.generate(task.copy(
            stack = StackEntry(
              target = or.TypeRepr.of[T],
              modifiers = _modifiers,
              path = _genPath,
              label = None,
              mode = generationMode
            ) :: task.stack
          ))

          new NestedResultImpl[T](raw)
        }
      }
    }

    def reportBuilder: ReportingBuilder = new ReportingBuilder {
      private var _plugin: PluginContainer = owner
      private var _stack: Seq[ReportingBuilder.StackEntry] = Vector.empty
      private var _defaultPos: Option[XPosition] = None

      def usePlugin(className: String): this.type = {
        _plugin = _plugins.getOrElse(className,
          throw new IllegalArgumentException(s"Plugin '$className' is not loaded"))

        this
      }

      def stackEntries(e: ReportingBuilder.StackEntry*): this.type = {
        _stack = _stack ++ e
        this
      }

      def defaultPosition(pos: Option[XPosition]): this.type = {
        _defaultPos = pos
        this
      }

      def build(): ErrorReporting = {
        val nextTask = GenerationTask(_stack.reverseIterator.foldLeft(task.stack) { (list, entry) =>
          StackEntry(TypeRepr.of(using entry.t), ModifierSet.empty, entry.path, entry.label, generationMode) :: list
        })

        outer.report.transform(
          msgTransformer = Some(formatMessage(_, nextTask, Some(_plugin))),
          defaultPos = _defaultPos
        )
      }
    }
  }

  private class ContextImpl(task: GenerationTask, owner: PluginContainer) extends BasicContextImpl(task, owner) {
    var generating: Boolean = false

    val outcome: GenerationOutcome = {
      val baseOutcome =
        try owner.instance.generate(modifiers)(using outer.q, this, generatedType)
        catch {
          case NonFatal(e) => reportException("Plugin threw an exception while producing generation outcome", e)
        }

      if (!generationMode.stringy || owner.capabilities(PluginCapability.SupportsStrings)) baseOutcome
      else if (!baseOutcome.isInstanceOf[GenerationCandidate]) baseOutcome
      else GenerationRejection("Plugin does not support generation of stringy formats")
    }

    val candidate: Option[GenerationCandidate] =
      outcome match {
        case c: GenerationCandidate => Some(c)
        case _ => None
      }

    export outer.{extensions, freshName, loadPlugin, plugins, symbolModifiers}

    def identity: AnyRef =
      try candidate.get.identity
      catch {
        case NonFatal(e) => reportException("Plugin failed to provide a serializer identity", e)
      }

    def doGeneration(quotes: Quotes): Term = {
      generating = true

      try candidate.get.generate(using quotes)().asTerm
      catch {
        case e: StopMacroExpansion => throw e
        case NonFatal(e) => reportException("Plugin failed to generate serializer code", e)
      }
    }
  }

  private def createHandle(task: GenerationTask, key: CachingKey, identity: AnyRef, simpleGen: Boolean,
                           gen: Quotes => Term): SerializerHandle =
  {
    val ret = new SerializerHandle

    ret.variableName = freshName("s" + task.typeSymbol.name)
    ret.recursiveWrapper = !simpleGen

    ret.serializedType = task.target
    ret.variableType =
      if (!ret.recursiveWrapper) generationMode.appliedType(task.target)
      else generationMode match {
        case GenerationMode.Decoder => TypeRepr.of[Recursive.Decoder].appliedTo(task.target)
        case GenerationMode.Encoder => TypeRepr.of[Recursive.Encoder].appliedTo(task.target)
        case GenerationMode.Format => TypeRepr.of[Recursive.Format].appliedTo(task.target)
      }

    ret.variableSymbol = Symbol.newVal(Symbol.spliceOwner, ret.variableName, ret.variableType, Flags.EmptyFlags,
      Symbol.noSymbol)

    ret.variableRef = Ref(ret.variableSymbol)
    ret.identity = identity

    // Store cache _before_ generation, so recursive types won't be regenerated infinitely
    _serializerSet.add(ret)
    _serializers.put(task.basicKey, ret)
    _serializers.put(key, ret)

    ForbiddenMacroUtils.clearQuotesCache()
    ret.definition = gen((if (ret.recursiveWrapper) Symbol.spliceOwner else ret.variableSymbol).asQuotes)

    // Store order _after_ generation, so nested types will get lower order
    ret.order = _serializerIndex
    _serializerIndex += 1

    ret
  }

  private def formatMessage(message: String, task: GenerationTask, plugin: Option[PluginContainer]): String = {
    val sb = new mutable.StringBuilder()
    sb ++= s"\u001b[1;31mWhile generating ${task.target.show(using Printer.TypeReprShortCode)}:\u001b[0m\n"

    sb ++= message
    if (!message.endsWith("\n")) sb += '\n'
    sb += '\n'

    // TODO: stack

    for (p <- plugin) {
      sb ++= s"\u001b[1;34mActive plugin:\u001b[0m ${p.name} (${p.className})\n"
    }

    val plugins = _pluginContainers.filterNot(_.className.startsWith("s3j."))
    if (plugins.nonEmpty) {
      sb ++= "\u001b[1;34mPresent plugins:\u001b[0m\n"
      for (p <- plugins) {
        sb ++= s" - ${p.name} (${p.className})"
      }
    }

    sb.result()
  }

  private def selectCandidate(task: GenerationTask, contexts: Set[ContextImpl], implicitError: String): ContextImpl =
  {
    if (!contexts.exists(c => c.outcome.isInstanceOf[GenerationCandidate])) {
      val sb = new mutable.StringBuilder()
      sb ++= "No plugin is available to generate a serializer for this type.\n"

      val rejections = contexts
        .filter(c => c.outcome.isInstanceOf[GenerationRejection])
        .map(c => c.owner -> c.outcome.asInstanceOf[GenerationRejection].message)

      for ((owner, msg) <- rejections) {
        sb ++= "\n - plugin '" ++= owner.name ++= "' (" ++= owner.className ++= ") rejected:\n"
        sb ++= "   " ++= msg ++= "\n"
      }

      sb ++= "\n\u001b[1;34mImplicit search result:\u001b[0m\n" ++= implicitError ++= "\n"

      report.errorAndAbort(formatMessage(sb.result(), task, None))
    }

    val certainCandidates = contexts.filter(c => c.candidate.exists(_.confidence.isCertain))
    if (certainCandidates.size > 1) {
      val sb = new mutable.StringBuilder()
      sb ++= "Multiple plugins produced conflicting serializer candidates:\n"

      for (c <- certainCandidates) {
        sb ++= "\n - plugin '" ++= c.owner.name ++= "' (" ++= c.owner.className ++= ")"
      }

      report.errorAndAbort(formatMessage(sb.result(), task, None))
    }

    if (certainCandidates.nonEmpty) {
      return certainCandidates.head
    }

    // Otherwise, select most confident candidate
    contexts
      .filter(c => c.candidate.isDefined)
      .maxBy(_.candidate.get.confidence.value)
  }

  private def runGenerationPlugins(task: GenerationTask, implicitError: String): SerializerHandle = {
    val context = selectCandidate(task, _pluginContainers.map(new ContextImpl(task, _)), implicitError)
    val key = PluginCachingKey(task.target, context.owner.className, context.identity)
    if (_serializers.contains(key)) {
      return _serializers(key)
    }

    createHandle(task, key, key.identity, simpleGen = context.candidate.exists(_.simpleGeneration),
      q => context.doGeneration(q).asInstanceOf[Term])
  }

  private def searchImplicits(t: TypeRepr, behaviors: Set[ImplicitBehavior]): AssistedImplicits.SearchResult = {
    val assistedHelper = generationMode match {
      case GenerationMode.Decoder | GenerationMode.StringDecoder =>
        Symbol.requiredModule("s3j.macros.codegen.AssistedHelpers.Decoder")

      case GenerationMode.Encoder | GenerationMode.StringEncoder =>
        Symbol.requiredModule("s3j.macros.codegen.AssistedHelpers.Encoder")

      case GenerationMode.Format | GenerationMode.StringFormat =>
        Symbol.requiredModule("s3j.macros.codegen.AssistedHelpers.Format")
    }

    AssistedImplicits.search(t, behaviors.flatMap(_.extraLocations), Some(assistedHelper))
  }

  private def generateImplicit(task: GenerationTask, r: AssistedImplicits.SearchSuccess)(using q: Quotes): Expr[Any] = {
    import q.reflect.*
    val holes = r.holes.map { tpe =>
      val (subMode, target) = GenerationMode.decode(TypeRepr.of(using tpe))
      if (!generationMode.modeCompatible(subMode)) {
        throw new RuntimeException("Implicit hole has an incompatible generation mode: mode=" + generationMode +
          ", hole=" + Type.show(using tpe))
      }

      val subTask = GenerationTask(
        stack = StackEntry(
          target = target.asInstanceOf[outer.q.reflect.TypeRepr],
          modifiers = task.modifiers,
          path = Nil,
          label = None,
          mode = subMode
        ) :: task.stack
      )

      generate(subTask).variableRef.asExpr
    }

    r.expr(holes)
  }

  def generateRoot(tpe: TypeRepr, modifiers: ModifierSet): Symbol =
    generate(GenerationTask(
      stack = StackEntry(tpe, modifiers, Nil, None, generationMode) :: Nil
    )).variableSymbol

  private def generate(task: GenerationTask): SerializerHandle = {
    if (_serializers.contains(task.basicKey)) {
      return _serializers(task.basicKey)
    }

    val implicitBehaviors: Set[(ImplicitBehavior, PluginContainer)] = _pluginContainers
      .map(c => c.instance.implicitBehavior(task.modifiers)(using q, this, task.targetType) -> c)

    val suppressImplicitsReason: Option[String] =
      if (task.isRoot || task.typeSymbol == typeSymbol) Some("serializer for root type is always generated")
      else if (implicitBehaviors.exists(_._1.suppressed)) {
        val reasons = implicitBehaviors
          .filter(_._1.suppressed)
          .map { case (_, pl) => s"${pl.name} (${pl.className})" }

        Some("suppressed by plugin: " + reasons.mkString(", "))
      } else None

    if (suppressImplicitsReason.nonEmpty) {
      return runGenerationPlugins(task, suppressImplicitsReason.get)
    }

    if (_serializers.contains(task.implicitKey)) {
      return _serializers(task.implicitKey)
    }

    searchImplicits(generationMode.appliedType(task.target), implicitBehaviors.map(_._1)) match {
      case success: AssistedImplicits.SearchSuccess =>
        createHandle(task, task.implicitKey, ImplicitIdentity, simpleGen = true,
          q => generateImplicit(task, success)(using q).asTerm)

      case AssistedImplicits.SearchFailure(explanation) =>
        if (task.modifiers.contains(BuiltinModifiers.requireImplicit)) {
          report.errorAndAbort(formatMessage("@requireImplicit is present and no implicit candidate was found:\n\n" +
            explanation, task, plugin = None))
        }

        runGenerationPlugins(task, explanation)
    }
  }
}
