package s3j.macros.codegen

import dotty.tools.dotc.ast.Trees.{Tree, Untyped}
import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.{StdNames, Symbols, Types}
import dotty.tools.dotc.printing.RefinedPrinter
import dotty.tools.dotc.printing.Texts.Text
import dotty.tools.dotc.typer.Implicits.ContextualImplicits
import dotty.tools.dotc.typer.{Implicits, ImportInfo}
import dotty.tools.dotc.util.Spans

import scala.annotation.{compileTimeOnly, tailrec, unused}
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}
import scala.quoted.runtime.impl.{ExprImpl, QuotesImpl, SpliceScope, TypeImpl}

/**
 * Helper for more flexible implicit search:
 *
 *  1. Allows to extend implicit search path with predefined locations
 *  1. Allows to match 'partial' implicits, where some implicit arguments are still missing. These arguments are
 *     later materialized by macros.
 */
object AssistedImplicits {
  sealed trait SearchResult {
    def show(using Quotes): String
  }

  /**
   * Successful implicit search. Some of arguments may not be found, and they are presented as [[holes]]. Final
   * expression could be obtained after filling these holes.
   *
   * @param holes Types which weren't found during implicit search
   * @param expr  Function to transform filled holes into final expression
   */
  case class SearchSuccess(holes: Seq[Type[_]], expr: Seq[Expr[Any]] => Expr[Any]) extends SearchResult {
    def show(using Quotes): String = s"SearchSuccess(holes=[${holes.map(t => Type.show(using t)).mkString(", ")}], " +
      s"expr=$expr)"
  }

  /** Failed implicit search */
  case class SearchFailure(explanation: String) extends SearchResult {
    def show(using Quotes): String = toString
  }

  /**
   * Perform assisted implicit search.
   *
   * @param target Target type to search
   * @param searchLocations Extra locations to consider, presented as module symbols
   * @param assistedHelper Special search location whose symbols are treated as 'holes'
   */
  def search(using q: Quotes)(target: q.reflect.TypeRepr, searchLocations: Set[q.reflect.Symbol],
                              assistedHelper: Option[q.reflect.Symbol]): SearchResult = {
    val qi: q.type & QuotesImpl = q.asInstanceOf[QuotesImpl & q.type]

    import qi.ctx
    val assistedMethods = assistedHelper
      .map(_.asInstanceOf[Symbol].namedType.allMembers.filter(_.name.toString.startsWith("$assisted")).toSet)
      .getOrElse(Set.empty)
      .map(_.symbol)

    def searchInner(extraLocations: Iterable[Symbol]): tpd.Tree = {
      val implicits = new ContextualImplicits(
        (searchLocations.map(_.asInstanceOf[Symbol]) ++ extraLocations)
          .flatMap { sym =>
            new ImportInfo(
              Symbols.newImportSymbol(qi.ctx.owner, tpd.Ident(sym.namedType), Spans.NoCoord),
              List(untpd.ImportSelector(untpd.Ident(StdNames.nme.EMPTY))),
              untpd.EmptyTree,
              isRootImport = false
            ).importedImplicits
          }
          .toList,
        qi.ctx.implicits,
        isImport = false
      )(qi.ctx)

      val subContext = qi.ctx.fresh.setImplicits(implicits)
      subContext.typer.inferImplicitArg(target.asInstanceOf[Types.Type],
        qi.reflect.Position.ofMacroExpansion.span)(using subContext)
    }

    @tailrec
    def isAssistedMethod(t: tpd.Tree): Boolean =
      t match {
        case tpd.TypeApply(arg, _) => isAssistedMethod(arg)
        case i: tpd.Ident => assistedMethods(i.tpe.termSymbol)
        case _ => false
      }

    def process(tree: tpd.Tree): SearchResult = tree.tpe match {
      case _: Implicits.SearchFailureType =>
        SearchFailure(tree.tpe.asInstanceOf[Implicits.SearchFailureType].explanation)

      case _ =>
        var holes = Vector.empty[tpd.Tree]
        val holeIdx = new mutable.HashMap[Types.Type, Int]

        val treeWithHoles = new tpd.TreeMap() {
          override def transform(tree: tpd.Tree)(using Context): tpd.Tree = {
            if (!isAssistedMethod(tree)) super.transform(tree)
            else {
              val typeTree = tpd.TypeTree(tree.tpe)
              val idx = holeIdx.getOrElseUpdate(tree.tpe, {
                holes :+= typeTree
                holes.size - 1
              })

              // Reuse Hole class in a bit of unusual way:
              tpd.Hole(false, idx, Nil, tpd.EmptyTree, typeTree)
            }
          }
        }.transform(tree)

        val holeTypes = holes.map(h => new TypeImpl(h, SpliceScope.getCurrent))
        val exprFn = new Function[Seq[Expr[Any]], Expr[Any]] {
          def apply(holeFillers: Seq[Expr[Any]]): Expr[Any] = {
            if (holeFillers.size != holes.size) {
              throw new IllegalArgumentException(s"Number of holes (${holes.size}) does not correspond to number of " +
                s"hole fillers (${holeFillers.size})")
            }

            val result = new tpd.TreeMap() {
              override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match {
                case h: tpd.Hole => holeFillers(h.idx).asInstanceOf[ExprImpl].tree
                case t => super.transform(t)
              }
            }.transform(treeWithHoles)

            new ExprImpl(result, SpliceScope.getCurrent)
          }

          override def toString(): String = {
            import dotty.tools.dotc.core.Decorators.show
            treeWithHoles
              .toText(new RefinedPrinter(qi.ctx) {
                override def toTextCore[T >: Untyped](tree: Tree[T]): Text = tree match {
                  case h: Trees.Hole[_] => literalText(s"hole${h.idx}")
                  case _ => super.toTextCore(tree)
                }
              })
              .mkString(Int.MaxValue, false)
          }
        }

        SearchSuccess(holeTypes, exprFn)
    }

    val outerResult = searchInner(assistedHelper.map(_.asInstanceOf[Symbol]))
    if (assistedHelper.isEmpty || !isAssistedMethod(outerResult)) process(outerResult)
    else process(searchInner(Nil))
  }
}
