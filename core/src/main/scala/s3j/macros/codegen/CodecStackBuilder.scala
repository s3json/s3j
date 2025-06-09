package s3j.macros.codegen

import s3j.internal.macros.ClassGenerator
import s3j.internal.macros.ClassGenerator.FieldHandle
import s3j.macros.FreshPluginContext.StackHandle

import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}

private[macros] class CodecStackBuilder(using q: Quotes) {
  import q.reflect.*

  private abstract class StackEntry {
    def name: String
    def field: Option[FieldHandle[?]]
    def definition: Option[Term]
    def staticType: Type[?]
    def externalReference: Expr[?]
    def symbol: Symbol
    def ensureField: FieldHandle[?]

    // Reference analysis state:

    var eagerDeps: Set[StackEntry] = Set.empty
    var lateDeps: Set[StackEntry] = Set.empty
    var incomingDeps: Set[StackEntry] = Set.empty
    var reachable: Boolean = false
    var rootReachable: Boolean = false
    var eagerComplete: Boolean = false
    var eagerOrder: Int = 0

    override def toString: String = {
      val sb = new mutable.StringBuilder()
      sb ++= "'" ++= name ++= "' [" ++= Type.show(using staticType) ++= "] = {\n"
      sb ++= "  eagerDeps=[" ++= eagerDeps.map(_.name).mkString(", ") ++= "],\n"
      sb ++= "  lateDeps=[" ++= lateDeps.map(_.name).mkString(", ") ++= "],\n"
      sb ++= "  incomingDeps=[" ++= incomingDeps.map(_.name).mkString(", ") ++= "],\n"
      sb ++= "  reachable=" ++= reachable.toString ++= ",\n"
      sb ++= "  rootReachable=" ++= rootReachable.toString ++= ",\n"
      sb ++= "  eagerComplete=" ++= eagerComplete.toString ++= ",\n"
      sb ++= "  eagerOrder=" ++= eagerOrder.toString ++= "\n"
      sb ++= "}"
      sb.result()
    }
  }

  private class TypedStackEntry[T](val name: String, val index: Int)(using val staticType: Type[T])
  extends StackEntry { outer =>
    var field:        Option[FieldHandle[T]] = None
    var definition:   Option[Term] = None

    val handle: StackHandle[T] = new StackHandle[T] {
      val staticType: Type[T] = outer.staticType

      def setDefinition(value: Expr[T]): Unit = {
        val valueTpe = value.asTerm.tpe

        if (!(valueTpe <:< TypeRepr.of[T])) {
          throw new IllegalAccessException("setDefinition expr does not match configured static type: " + Type.show[T])
        }

        type U <: T
        given Type[U] = valueTpe.asType.asInstanceOf[Type[U]]

        val f = ensureFieldT[U]
        f.setInitializer(value)
        definition = Some(value.asTerm.changeOwner(f.symbol))
      }

      def nestedQuotes: Quotes = ensureFieldT[T].nestedQuotes
      def reference: Expr[T] = ensureFieldT[T].reference

      override def toString: String = s"StackHandle[${Type.show[T]}]($name)"
    }

    def externalReference: Expr[T] = ensureField.externalReference
    def symbol: Symbol = ensureField.symbol

    def ensureField: FieldHandle[T] = ensureFieldT[T]

    private def ensureFieldT[U <: T](using Type[U]): FieldHandle[T] =
      field.getOrElse {
        val finalName =
          if (usedNames.contains(name)) name + "_" + index
          else name

        usedNames.add(name)

        val result = classGen.defineField[U](finalName).asInstanceOf[FieldHandle[T]]
        field = Some(result)
        result
      }
  }

  private val entries: mutable.ArrayBuffer[StackEntry] = mutable.ArrayBuffer.empty
  private val usedNames: mutable.Set[String] = mutable.HashSet.empty
  private val classGen: ClassGenerator = ClassGenerator("$wrapper")

  private var nextIndex = 0
  private var symbolEntries: Map[Symbol, StackEntry] = _

  def addEntry[T](name: String)(using Type[T]): StackHandle[T] = {
    val entry = new TypedStackEntry(name, nextIndex)
    nextIndex += 1
    entries.append(entry)
    entry.handle
  }

  private def matchReference(expr: Term): Option[StackEntry] =
    expr.tpe match {
      case tr: TermRef => symbolEntries.get(expr.symbol)
      case _ => None
    }

  private def transformRootExpression(expr: Term): Term = {
    val treeMap = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        matchReference(tree) match {
          case Some(e) =>
            e.rootReachable = true
            e.externalReference.asTerm

          case None => super.transformTerm(tree)(owner)
        }
    }

    treeMap.transformTerm(expr)(Symbol.spliceOwner)
  }

  private def collectDependencies(): Unit = {
    def traverse(ownerEntry: StackEntry, eager: Boolean, tree: Tree): Unit = {
      val tt = new TreeTraverser {
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
          case dd: DefDef if eager => traverse(ownerEntry, eager = false, dd)
          case tree: Term => matchReference(tree) match {
            case Some(target) =>
              if (eager) ownerEntry.eagerDeps += target
              else ownerEntry.lateDeps += target
              target.incomingDeps += ownerEntry

            case None => super.traverseTree(tree)(owner)
          }

          case _ => super.traverseTree(tree)(owner)
        }
      }

      tt.traverseTree(tree)(ownerEntry.symbol)
    }

    for (e <- entries) {
      traverse(e, eager = true, e.definition.get)
    }
  }

  private def inferReachability(): Unit = {
    val q = new mutable.ArrayDeque[StackEntry]()

    for (e <- entries if e.rootReachable) {
      e.reachable = true
      q.append(e)
    }

    while (q.nonEmpty) {
      val e = q.removeHead()
      e.reachable = true

      for (next <- e.eagerDeps ++ e.lateDeps if !next.reachable) {
        q.append(next)
      }
    }
  }

  private def inferEagerOrdering(): Unit = {
    var nextOrder = 0

    for (e <- entries if e.eagerDeps.isEmpty) {
      e.eagerComplete = true
      e.eagerOrder = nextOrder
      nextOrder += 1
    }

    for (e <- entries if !e.reachable) {
      e.eagerComplete = true
    }

    var changed = true
    while (changed) {
      changed = false

      for (e <- entries if !e.eagerComplete && e.eagerDeps.forall(_.eagerComplete)) {
        changed = true
        e.eagerComplete = true
        e.eagerOrder = nextOrder
        e.ensureField.setPosition(e.eagerOrder)
        nextOrder += 1
      }
    }

    if (!entries.forall(_.eagerComplete)) {
      val sb = new mutable.StringBuilder
      sb ++= "Generated serializer stack has circular dependencies that prevented it from instantiation:\n"

      for (e <- entries if!e.eagerComplete) {
        sb ++= "- name: '" ++= e.name ++= "', type: " ++= Type.show(using e.staticType) ++= "\n"
        sb ++= "  dependencies: " ++= e.eagerDeps.map(_.name).mkString(", ") ++= "\n"
        sb ++= "  definition: " ++= e.definition.get.show.replaceAll("\n", "\n  ") ++= "\n"
      }

      report.errorAndAbort(sb.result())
    }
  }

  def result[X](expr: Expr[X])(using Type[X]): Expr[X] = {
    for (e <- entries if e.definition.isEmpty) {
      val typeName = Type.show(using e.staticType)
      throw new IllegalStateException(s"Could not complete codec stack: entry ${e.name}[$typeName] has no definition")
    }

    // All symbols are guaranteed to be set now.
    symbolEntries = entries.map(e => e.symbol -> e).toMap

    val transformedExpr = transformRootExpression(expr.asTerm).asExprOf[X]
    if (!entries.exists(_.rootReachable)) {
      // expr does not refer to any stack entry (i.e. everything has got inlined): just return it as-is
      return expr
    }

    collectDependencies()
    inferReachability()
    inferEagerOrdering()

    if (
      entries.count(_.reachable) == 1 &&
      entries.find(_.reachable).get.incomingDeps.isEmpty &&
      matchReference(expr.asTerm).isDefined
    ) {
      // We can avoid creation of wrapper class if:
      // 1. just one entry is reachable (otherwise class is necessary to hold two or more entries)
      // 2. if it does not have recursive references (otherwise class is necessary to link them)
      // 3. if 'expr' is just a reference to it (TODO: Transform expr to a block otherwise)
      return entries.find(_.reachable).get.definition.get.asExprOf[X]
    }

    classGen.build(transformedExpr)
  }
}
