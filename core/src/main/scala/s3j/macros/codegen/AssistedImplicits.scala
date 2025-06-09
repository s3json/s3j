package s3j.macros.codegen

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, StringyDecoder, StringyEncoder, StringyFormat}
import s3j.internal.macros.ExtendedImplicitSearch
import s3j.macros.codegen.AssistedImplicits.{SearchFailure, SearchResult, SearchSuccess}
import s3j.macros.generic.GenerationMode
import s3j.schema.JsonSchema

import scala.annotation.{compileTimeOnly, unused}
import scala.quoted.{Expr, Quotes, Type}

object AssistedImplicits {
  sealed trait AssistedHelper[C[_] >: Null] {
    @compileTimeOnly("Assisted implicit helper was not removed by the macros")
    given $assisted[T]: C[T] = null /* removed by macro processing */
  }

  @unused object EncoderHelper extends AssistedHelper[JsonEncoder]
  @unused object DecoderHelper extends AssistedHelper[JsonDecoder]
  @unused object FormatHelper extends AssistedHelper[JsonFormat]
  @unused object StringEncoderHelper extends AssistedHelper[StringyEncoder]
  @unused object StringDecoderHelper extends AssistedHelper[StringyDecoder]
  @unused object StringFormatHelper extends AssistedHelper[StringyFormat]
  @unused object SchemaHelper extends AssistedHelper[JsonSchema]

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

  def result: SearchResult = {
    val result: ImplicitSearchResult = ExtendedImplicitSearch.instance.search(
      targetType,
      position,
      Symbol.requiredModule(s"s3j.macros.codegen.AssistedImplicits.${mode.name}Helper") +: extraLocations
    )

    result match {
      case s: ImplicitSearchSuccess if matchAssistedHelper(s.tree).isDefined =>
        SearchFailure(s"no implicit value found for ${targetType.show}")

      case s: ImplicitSearchSuccess => SuccessResultImpl(s.tree)
      case f: ImplicitSearchFailure => SearchFailure(f.explanation)
    }
  }

  private def matchAssistedHelper(tree: q.reflect.Tree): Option[q.reflect.TypeRepr] =
    tree match {
      case TypeApply(base: Ident, List(arg)) =>
        val isHelper = base.symbol.fullName == "s3j.macros.codegen.AssistedImplicits$.AssistedHelper.$assisted"
        if (isHelper) Some(arg.tpe)
        else None

      case _ => None
    }

  private class SuccessResultImpl(tree: Tree) extends SearchSuccess {
    private val _missingTypes: Vector[q.reflect.TypeRepr] = scanMissingTypes()

    private def scanMissingTypes(): Vector[q.reflect.TypeRepr] = {
      var result: Vector[q.reflect.TypeRepr] = Vector.empty

      class Traverser extends TreeTraverser {
        override def traverseTree(tree: q.reflect.Tree)(owner: q.reflect.Symbol): Unit =
          matchAssistedHelper(tree) match {
            case Some(arg) =>
              if (!result.exists(_ =:= arg)) {
                result :+= arg
              }

            case _ => super.traverseTree(tree)(owner)
          }
      }

      Traverser().traverseTree(tree)(Symbol.spliceOwner)

      result
    }

    def missingTypes: Seq[Type[?]] =
      _missingTypes.map(t => mode.appliedType(t).asType)

    def result(missingValues: Seq[Expr[?]]): Expr[Any] = {
      if (missingValues.size != _missingTypes.size) {
        throw new IllegalAccessException("Mismatched count of missing values")
      }

      class Mapper extends TreeMap {
        override def transformTerm(tree: q.reflect.Term)(owner: q.reflect.Symbol): q.reflect.Term =
          matchAssistedHelper(tree) match {
            case Some(arg) =>
              val typeIndex = _missingTypes.indexWhere(_ =:= arg)
              if (typeIndex < 0) {
                throw new RuntimeException("Type not found when performing substitution")
              }

              missingValues(typeIndex).asTerm.changeOwner(owner)

            case None => super.transformTerm(tree)(owner)
          }
      }

      Mapper().transformTree(tree)(Symbol.spliceOwner).asExpr
    }
  }
}
