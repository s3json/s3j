package s3j.macros.generic

import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.format.util.Recursive
import s3j.macros.GenerationContext.{GenerationCandidate, GenerationOutcome, GenerationRejection}
import s3j.macros.codegen.Position as XPosition
import s3j.macros.{GenerationContext, PluginContext}
import s3j.macros.modifiers.{BuiltinModifiers, ModifierSet}
import s3j.macros.traits.{ErrorReporting, NestedBuilder, NestedResult, ReportingBuilder}
import s3j.macros.utils.{GenerationPath, MacroUtils, ReportingUtils}

import java.io.{PrintWriter, StringWriter}
import scala.util.control.NonFatal
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}
import scala.quoted.runtime.StopMacroExpansion

private[macros] trait GenerationContextImpl { outer: PluginContextImpl[_] =>
  import q.reflect.*
  import q.{reflect => or}

  private var _serializerIndex: Int = 0

  private case object ImplicitIdentity
  case class StackEntry(target: TypeRepr, modifiers: ModifierSet, path: Seq[GenerationPath], label: Option[String])

  case class GenerationTask(stack: List[StackEntry]) {
    val isRoot: Boolean = stack.tail.isEmpty
    val target: TypeRepr = stack.head.target
    val typeSymbol: Symbol = target.typeSymbol
    val modifiers: ModifierSet = stack.head.modifiers

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

    val generationMode: GenerationMode = outer.generationMode
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
              label = None
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
          StackEntry(TypeRepr.of(using entry.t), ModifierSet.empty, entry.path, entry.label) :: list
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

    val outcome: GenerationOutcome =
      try owner.instance.generate(modifiers)(using outer.q, this, generatedType)
      catch {
        case NonFatal(e) => reportException("Plugin threw an exception while producing generation outcome", e)
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

    val definiteCandidates = contexts.filter(c => c.candidate.exists(_.confidence.isEmpty))
    if (definiteCandidates.size > 1) {
      val sb = new mutable.StringBuilder()
      sb ++= "Multiple plugins produced conflicting serializer candidates:\n"

      for (c <- definiteCandidates) {
        sb ++= "\n - plugin '" ++= c.owner.name ++= "' (" ++= c.owner.className ++= ")"
      }

      report.errorAndAbort(formatMessage(sb.result(), task, None))
    }

    if (definiteCandidates.nonEmpty) {
      return definiteCandidates.head
    }

    // Otherwise, select most confident candidate
    contexts
      .filter(c => c.candidate.exists(_.confidence.nonEmpty))
      .maxBy(_.candidate.get.confidence.get)
  }

  private def runGenerationPlugins(task: GenerationTask, implicitError: String): SerializerHandle = {
    val context = selectCandidate(task, _pluginContainers.map(new ContextImpl(task, _)), implicitError)
    val key = PluginCachingKey(task.target, context.owner.className, context.identity)
    if (_serializers.contains(key)) {
      return _serializers(key)
    }

    createHandle(task, key, key.identity, simpleGen = false, q => context.doGeneration(q).asInstanceOf[Term])
  }

  private def searchImplicits(t: TypeRepr): ImplicitSearchResult = {
    // TODO: Find a way to augment the search path
    Implicits.search(t)
  }

  def generateRoot(tpe: TypeRepr, modifiers: ModifierSet): Symbol =
    generate(GenerationTask(
      stack = StackEntry(tpe, modifiers, Nil, None) :: Nil
    )).variableSymbol

  private def generate(task: GenerationTask): SerializerHandle = {
    if (_serializers.contains(task.basicKey)) {
      return _serializers(task.basicKey)
    }

    val suppressImplicitsReason: Option[String] =
      if (task.isRoot || task.typeSymbol == typeSymbol) Some("serializer for root type is always generated")
      else if (task.modifiers.values.exists(_.suppressImplicitSearch)) {
        val mods = task.modifiers.values.filter(_.suppressImplicitSearch)
        Some("suppressed by modifiers: " + mods.mkString(", "))
      } else None

    if (suppressImplicitsReason.nonEmpty) {
      return runGenerationPlugins(task, suppressImplicitsReason.get)
    }

    if (_serializers.contains(task.implicitKey)) {
      return _serializers(task.implicitKey)
    }

    searchImplicits(generationMode.appliedType(task.target)) match {
      case success: ImplicitSearchSuccess =>
        createHandle(task, task.implicitKey, ImplicitIdentity, simpleGen = true, _ => success.tree)

      case failure: ImplicitSearchFailure =>
        if (task.modifiers.contains(BuiltinModifiers.requireImplicit)) {
          report.errorAndAbort(formatMessage("@requireImplicit is present and no implicit candidate was found:\n\n" +
            failure.explanation, task, plugin = None))
        }

        runGenerationPlugins(task, failure.explanation)
    }
  }
}
