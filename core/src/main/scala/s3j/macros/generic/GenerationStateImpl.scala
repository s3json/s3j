package s3j.macros.generic

import s3j.format.{JsonEncoder, JsonDecoder}
import s3j.macros.GenerationContext
import s3j.macros.modifiers.ModifierSet
import s3j.macros.schema.SchemaExpr
import s3j.macros.traits.NestedResult

import java.lang.reflect.InvocationTargetException
import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.{Flags as DFlags, Symbols, Types}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Decorators._

private[macros] trait GenerationStateImpl { this: PluginContextImpl[_] =>
  import q.reflect.*

  private class NestedResultImpl[T](handle: SerializerHandle)(using Type[T]) extends NestedResult[T] {
    def raw(using q: Quotes): Expr[Any] = {
      import q.reflect.*
      Ref(handle.variableSymbol.asInstanceOf[Symbol]).asExpr
    }

    def encoder(using Quotes): Expr[JsonEncoder[T]] = {
      if (!handle.generationMode.generateEncoders) {
        throw new IllegalStateException("Encoders are not generated in mode " + generationMode)
      }

      raw.asExprOf[JsonEncoder[T]]
    }

    def decoder(using Quotes): Expr[JsonDecoder[T]] = {
      if (!handle.generationMode.generateDecoders) {
        throw new IllegalStateException("Decoders are not generated in mode " + generationMode)
      }

      raw.asExprOf[JsonDecoder[T]]
    }

    def schema: SchemaExpr[T] = {
      if (handle.generationMode != GenerationMode.Schema) {
        throw new IllegalStateException("Schema is not generated in mode " + generationMode)
      }

      handle.schemaDefinition.asInstanceOf[SchemaExpr[T]]
    }
  }

  // Caching of generated serializers is absolutely necessary:
  // 1) for performance reasons to avoid generating same serializers from scratch
  // 2) for code bloat reasons to reuse equivalent serializers
  // 3) to handle recursive types

  // Caching consists of two layers:
  // 1) core layer comparing type and it's modifiers - cheap layer that could be used right from start
  // 2) plugin layer comparing some plugin-specific ('semantical') identities. These identites could be costly to
  //    generate (they could require parsing of everything just as during generation), but they could lead to much more
  //    precise cache hits

  protected class SerializerHandle {
    var generationMode: GenerationMode = _
    var targetType: TypeRepr = _
    var variableName: String = _
    var variableType: TypeRepr = _
    var variableSymbol: Symbol = _
    var definition: Term = _
    var schemaDefinition: SchemaExpr[?] = _
    var identity: AnyRef = _

    // Schemas use delayed initialization to have most precise type, including possible inline schemas
    // Such precise type isn't available until schema is actually generated, so variable isn't created right away.
    // But for recursive schemas we will hit `reference` method before generation is finished - in this case
    // type is initialized as generic `JsonSchema[T]` - recursive schemas can't be inlined anyway.

    def initialized: Boolean = variableType != null

    def initializeDefault(): Unit = {
      initialize(generationMode.appliedType(targetType))
    }

    def storeSchema(sch: SchemaExpr[?]): Unit = {
      if (!initialized) {
        initialize(TypeRepr.of(using SchemaExpr.toType(sch)))
      }

      schemaDefinition = sch

      if (definition == null) {
        definition = SchemaExpr.toExpr(sch, inlineTypes = false)(using variableSymbol.asQuotes).asTerm
      }
    }

    def initialize(tpe: TypeRepr): Unit = {
      if (initialized) {
        return
      }

      variableType = tpe
      variableSymbol = Symbol.newVal(_classSym, variableName, variableType, Flags.EmptyFlags, Symbol.noSymbol)
    }

    def reference: Term = {
      if (!initialized) {
        initializeDefault()
      }

      Ref(variableSymbol)
    }

    def toNestedResult[T](using Type[T]): NestedResult[T] = new NestedResultImpl[T](this)
  }

  protected sealed trait CachingKey
  protected case class BasicCachingKey(target: TypeRepr, modifiers: ModifierSet) extends CachingKey
  protected case class ImplicitCachingKey(target: TypeRepr) extends CachingKey
  protected case class PluginCachingKey(target: TypeRepr, plugin: String, identity: AnyRef) extends CachingKey

  protected val _serializers: mutable.Map[CachingKey, SerializerHandle] = mutable.HashMap.empty
  protected var _serializerSet: mutable.Set[SerializerHandle] = mutable.HashSet.empty
  protected var _serializerIdx: Int = 0

  @threadUnsafe
  protected lazy val _classSym: Symbol = {
    import qi.ctx

    Symbols.newNormalizedClassSymbol(qi.ctx.owner, "$wrapper".toTypeName, DFlags.Final, List(Symbols.defn.ObjectType),
      Types.NoType, Symbols.NoSymbol).asInstanceOf[Symbol]
  }

  private class RefAnalysisState {
    var eagerDeps: Set[Symbol] = Set.empty
    var lateDeps: Set[Symbol] = Set.empty
    var reachable: Boolean = false
    var eagerComplete: Boolean = false
    var order: Int = 0

    override def toString: String = {
      val sb = new mutable.StringBuilder()
      sb ++= "{\n"
      sb ++= "  eagerDeps=[" ++= eagerDeps.map(_.name).mkString(", ") ++= "],\n"
      sb ++= "  lateDeps=[" ++= lateDeps.map(_.name).mkString(", ") ++= "],\n"
      sb ++= "  reachable=" ++= reachable.toString ++= ",\n"
      sb ++= "  eagerComplete=" ++= eagerComplete.toString ++= ",\n"
      sb ++= "  order=" ++= order.toString ++= "\n"
      sb ++= "}"
      sb.result()
    }
  }

  private def analyzeReferences(root: Symbol): Map[Symbol, RefAnalysisState] = {
    import qi.ctx
    val ret = _serializerSet.map(h => h.variableSymbol -> new RefAnalysisState).toMap

    def traverse(caller: Symbol, eager: Boolean, tree: Tree): Unit = {
      val cst = ret(caller)
      val tr = new TreeTraverser {
        override def traverseTree(tree: q.reflect.Tree)(owner: q.reflect.Symbol): Unit = {
          tree match {
            case s: Select =>
              s.tpe match {
                case tr: TermRef =>
                  val sym = tr.asInstanceOf[Types.NamedType].symbol.asInstanceOf[Symbol]
                  if (ret.contains(sym)) {
                    if (eager) cst.eagerDeps += sym
                    else cst.lateDeps += sym
                  }
              }

              super.traverseTree(s.qualifier)(owner)

            // assume late dependency when we cross method boundary:
            case m: DefDef if eager => traverse(caller, eager = false, m)

            case _ => super.traverseTree(tree)(owner)
          }
        }
      }

      tr.traverseTree(tree)(caller)
    }

    for (h <- _serializerSet) {
      traverse(h.variableSymbol, eager = true, h.definition)
    }

    // Get reachability;
    val reachQ = new mutable.ArrayDeque[Symbol]()
    reachQ.append(root)
    while (reachQ.nonEmpty) {
      val st = ret(reachQ.removeHead())
      st.reachable = true

      for (next <- st.eagerDeps ++ st.lateDeps if !ret(next).reachable) {
        reachQ.append(next)
      }
    }

    // Get eager init ordering:
    var changed = true
    var order = 0

    for ((_, st) <- ret) {
      st.eagerComplete = st.eagerDeps.isEmpty
      st.order = order
      order += 1
    }

    while (changed) {
      changed = false

      for ((_, st) <- ret if !st.eagerComplete) {
        st.eagerComplete = st.eagerDeps.forall(s => ret(s).eagerComplete)
        if (st.eagerComplete) {
          st.order = order
          order += 1
          changed = true
        }
      }
    }

    // Check for cycles:
    if (!ret.forall(_._2.eagerComplete)) {
      val sb = new mutable.StringBuilder
      sb ++= "Generated serializer set has circular dependencies that prevented it from instantiation:\n"

      val symHandles = _serializerSet.map(h => h.variableSymbol -> h).toMap
      for (h <- _serializerSet; st = ret(h.variableSymbol) if st.reachable && !st.eagerComplete) {
        sb ++= "- type: " ++= h.targetType.show ++= " (wrapped: " ++= h.variableType.show ++= ")\n"
        sb ++= "  dependencies: " ++= st.eagerDeps.map(s => symHandles(s).targetType.show).mkString(", ") ++= "\n"
      }

      report.errorAndAbort(sb.result())
    }

    ret
  }

  def buildResult(root: Symbol): Term = {
    import qi.ctx
    val refs = analyzeReferences(root)
    if (refs.count(_._2.reachable) == 1) {
      return _serializerSet.find(_.variableSymbol == root).get.definition
    }

    val classBody = Vector.newBuilder[Statement]

    val cls = _classSym.asInstanceOf[Symbols.ClassSymbol]
    cls.enter(Symbols.newConstructor(cls, DFlags.Synthetic, Nil, Nil))

    val orderedSerializers = _serializerSet.iterator
      .filter(h => refs(h.variableSymbol).reachable)
      .toVector.sortBy(h => refs(h.variableSymbol).order)

    for (s <- orderedSerializers) {
      cls.enter(s.variableSymbol.asInstanceOf[Symbols.Symbol])
      classBody += ValDef(s.variableSymbol, Some(s.definition))
    }

    val untpdCtr = untpd.DefDef(nme.CONSTRUCTOR, Nil, tpd.TypeTree(Symbols.defn.UnitClass.typeRef), tpd.EmptyTree)
    val classDef = tpd.ClassDefWithParents(cls, ctx.typeAssigner.assignType(untpdCtr, cls.primaryConstructor),
      List(TypeTree.of[AnyRef].asInstanceOf[tpd.TypeTree]), classBody.result().toList.asInstanceOf[List[tpd.Tree]])

    Block(
      List(classDef.asInstanceOf[ClassDef]),
      Select(Apply(Select(New(TypeIdent(_classSym)), _classSym.primaryConstructor), Nil), root)
    )
  }
}
