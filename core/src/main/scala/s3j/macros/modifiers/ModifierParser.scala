package s3j.macros.modifiers

import s3j.macros.modifiers.ModifierParser.StoredModifier
import s3j.macros.utils.QualifiedName

import scala.annotation.Annotation
import scala.quoted.{Expr, Quotes, Type}

object ModifierParser {
  /** Stored (serialized) form of the modifier */
  sealed trait StoredModifier {
    /** @return Fully qualified name of modifier type */
    def typeName: String

    /** @return Context of modifier occurrence */
    def context: ModifierContext
  }

  /** Modifier stored as annotation */
  trait AnnotationModifier extends StoredModifier {
    /** Actual type of annotation */
    def annotation: Type[_ <: Annotation]

    /** @return Type arguments for annotation */
    def typeArgs: List[Type[_]]

    /** @return Arguments for annotation */
    def args: List[Expr[Any]]
  }
  
  /** Modifier stored as text, e.g. in settings */
  trait TextModifier extends StoredModifier {
    /** @return Text content of the modifier */
    def content: String
  }

  type ParseFunction = Quotes ?=> PartialFunction[StoredModifier, Modifier]

  /** Mutable class for convenient parser construction */
  trait ParserBuilder {
    /** Parse annotation of type `T` with user function */
    def parseFn[T](parser: ParseFunction)(using QualifiedName[T]): ParserBuilder

    /** Parse annotation of type `T`, returning singleton modifier instance */
    def parse[T](modifier: Modifier)(using QualifiedName[T]): ParserBuilder

    /** Parse annotation of named type with user function */
    def parseFn(name: String)(parser: ParseFunction): ParserBuilder

    /** Parse annotation of named type, returning singleton modifier instance */
    def parse(name: String, modifier: Modifier): ParserBuilder

    /** @return Built modifier parser */
    def build(): ModifierParser
  }

  /** [[ModifierParser]] instance which has no supported modifiers */
  val empty: ModifierParser =
    new ModifierParser {
      def supportedTypes: Set[String] = Set.empty
      def apply(using q: Quotes)(mod: StoredModifier): Modifier =
        throw new UnsupportedOperationException("ModifierParser.empty.apply()")
    }

  /** @return New parser builder instance */
  def builder: ParserBuilder = new ParserBuilderImpl
}

trait ModifierParser {
  /** @return set of recognized types for stored modifiers */
  def supportedTypes: Set[String]

  /** Parse stored modifier into runtime modifier object */
  def apply(using Quotes)(mod: StoredModifier): Modifier
}
