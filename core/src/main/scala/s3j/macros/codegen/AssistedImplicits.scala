package s3j.macros.codegen

import s3j.internal.macros.{AssistedSearchResult, ExtendedImplicitSearch}
import s3j.macros.codegen.AssistedImplicits.{SearchFailure, SearchResult, SearchSuccess}
import s3j.macros.generic.GenerationMode

import scala.quoted.{Expr, Quotes, Type}

object AssistedImplicits {
  sealed trait SearchResult
  case class SearchFailure(explanation: String) extends SearchResult
  sealed trait SearchSuccess extends SearchResult {
    def missingTypes: Seq[Type[?]]
    def result(missingValues: Seq[Expr[?]]): Expr[Any]
  }
}

private[macros] class AssistedImplicits(using val q: Quotes)(
  mode:           GenerationMode,
  targetType:     q.reflect.TypeRepr,
  position:       q.reflect.Position,
  extraLocations: Seq[q.reflect.Symbol],
) {
  import q.reflect.{*, given}

  /** Type class of the current generation mode: only its instances are assisted */
  private val assistedClass: Symbol = mode.appliedType(TypeRepr.of[Any]).typeSymbol

  def result: SearchResult =
    ExtendedImplicitSearch.instance.search(targetType, position, extraLocations, _.typeSymbol == assistedClass) match {
      case s: AssistedSearchResult.Success => SuccessResultImpl(s)
      case f: AssistedSearchResult.Failure => SearchFailure(f.explanation)
    }

  private class SuccessResultImpl(r: AssistedSearchResult.Success) extends SearchSuccess {
    def missingTypes: Seq[Type[?]] = r.missingTypes.map(_.asType)

    def result(missingValues: Seq[Expr[?]]): Expr[Any] =
      r.construct(missingValues.map(_.asTerm).toList).asExpr
  }
}
