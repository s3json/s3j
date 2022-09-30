package s3j.core.casecls

import s3j.core.casecls.CaseClassContext.{ObjectModel, ObjectModelBuilder}
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.GenerationContext
import s3j.macros.codegen.{NameGenerator, Variable}
import s3j.macros.modifiers.ModifierSet
import s3j.macros.traits.{BasicPluginContext, NestedBuilder}
import s3j.macros.utils.{GenerationPath, VariableHelper}

import scala.quoted.{Expr, Quotes, Type}

object CaseClassContext {
  /**
   * Request to generate [[ObjectField]] representation for a case class field.
   *
   * @param fieldName      Raw field name (as written in code)
   * @param key            Field key
   * @param fieldType      Type of the field
   * @param ownModifiers   Modifiers of the field (no inherited modifiers applied)
   * @param baseModifiers  Modifiers of the case class
   */
  case class FieldRequest[T](fieldName: String, key: String, fieldType: Type[T], ownModifiers: ModifierSet,
                             baseModifiers: ModifierSet)
  {
    /** Implicit type for the field */
    given implicitType: Type[T] = fieldType

    /** Field modifiers with inherited modifiers applied */
    val fieldModifiers: ModifierSet = baseModifiers ++ ownModifiers

    /** @return Type symbol for a field type */
    def typeSymbol(using q: Quotes): q.reflect.Symbol = {
      import q.reflect.*
      TypeRepr.of(using fieldType).typeSymbol
    }
  }

  /** Generic environment for code generation */
  trait GenerationEnvironment extends NameGenerator {
  }

  trait DecodingEnvironment extends GenerationEnvironment {
    /** @return Code which checks the presence of (statically known) key and throws if key is missing */
    def checkRequiredKey(key: String)(using Quotes): Expr[Unit]

    /** @return Code which checks the presence of (statically known) key and returns boolean result */
    def isKeyPresent(key: String)(using Quotes): Expr[Boolean]
  }

  /**
   * Generated code with various parts to be inserted into overall result.
   *
   * Each part will be called once (for given arguments). Results will be placed into final code like this
   * (details simplified for clarity):
   *
   * {{{
   *   case class X(..., someField: Y, ...)
   *
   *   new JsonDecoder[X] {
   *     def decode(reader: JsonReader): X = {
   *       ${ ...decodeVariables }
   *
   *       /* decoding loop */ {
   *         reader.readKey match {
   *           case "foo" => ${ decodeKey("foo", '{ reader }) }
   *           case "moo" => ${ decodeKey("moo", '{ reader }) }
   *           case other => ${ decodeDynamicKey('{ other }, '{ reader }) }
   *         }
   *       }
   *
   *       X(..., ${ decodeResult }, ...)
   *     }
   *   }
   * }}}

   * 2. `decodeVariables` should declare necessary variables to hold decoding state, along with their initializers.
   * 3. `decodeKey` will be called for each key defined in [[ObjectField.handledKeys]]. It should update state variables
   * to reflect decoded result.
   * 4. `decodeDynamicKey` will be called if [[ObjectField.handlesDynamicKeys]] is set. It corresponds to situations
   * where key is not matched by set of static keys.
   * 5. `decodeResult` should compose all state variables into final value, which will be returned to user.
   */
  trait DecodingCode {
    /** Variables to be defined in the beginning of decoding phase. */
    def usedVariables: Seq[Variable[_]]

    /**
     * Code for decoding a specific statically known key from reader.
     * Called for all keys in [[ObjectField.handledKeys]].
     */
    def decodeKey(key: String, reader: Expr[JsonReader])(using Quotes): Expr[Any]

    /**
     * Code for decoding a dynamic key stored in variable from reader.
     * Called only if [[ObjectField.handlesDynamicKeys]] is true.
     */
    def decodeDynamicKey(key: Expr[String], reader: Expr[JsonReader])(using Quotes): Expr[Any] =
      throw new UnsupportedOperationException("decodeDynamicKey")

    /**
     * Code to run after decoding loop is finished and just before result is going to be assembled (i.e. to check
     * presence of required keys, to fill default values...)
     */
    def decodeFinal()(using Quotes): Expr[Any]

    /** Code for assembling decoding state into a result */
    def decodeResult()(using Quotes): Expr[Any]
  }

  /** Model describing a serialization scheme for a single field */
  trait ObjectField[T] {
    /** Identity of this field group */
    def identity: AnyRef

    /** @return Set of statically known handled keys */
    def handledKeys: Set[String]

    /** @return true if this field handles dynamic keys (only one field in object could do that) */
    def handlesDynamicKeys: Boolean

    /**
     * Generate encoding code for the field.
     *
     * @param env    Generation environment. Splice will be owned by encoding method
     * @param writer Writer instance to use. Object will be already started, writing must start with a key.
     * @param value  Value to encode
     * @return       Generated code to encode this field
     */
    def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])
                       (using Quotes, GenerationEnvironment): Expr[Any]

    /**
     * Generate decoding code for the field.
     *
     * @param env    Decoding environment. Splice will be owned by decoding method
     * @return       Generated code to decode this field
     */
    def generateDecoder(using Quotes, DecodingEnvironment): DecodingCode
  }

  /** Model describing a serialization scheme for an object. Acts as a big object field producing the whole object. */
  trait ObjectModel[T] extends ObjectField[T] {
    /** List of fields included in object */
    def fields: Seq[ObjectField[?]]

    /** Field handling dynamic keys, if any */
    def dynamicField: Option[ObjectField[?]]
  }

  // Generation outcomes for case classes:

  /** Outcome for case class generation */
  sealed trait GenerationOutcome[+T]

  /** Outcome signalling that plugin is unable to do generation */
  case object GenerationUnsupported extends GenerationOutcome[Nothing]

  /** Positive outcome for case class generation */
  trait GenerationCandidate[T] extends GenerationOutcome[T] {
    /** @return Confidence for the candidate (confidence logic follows overall plugin cofidence logic) */
    def confidence: Option[Int]

    /** @return Materialized object field */
    def result: ObjectField[T]
  }

  trait ObjectModelBuilder[T] {
    /** Discard existing modifiers, leaving out and empty set */
    def discardModifiers(): this.type

    /** Add modifiers on top of existing set */
    def modifiers(mods: ModifierSet): this.type

    /** Add generation path */
    def generationPath(p: GenerationPath*): this.type

    /** Add label to stack entry */
    def label(s: String): this.type

    /** Build object model for specified settings */
    def build(): ObjectModel[T]
  }
}

trait CaseClassContext extends BasicPluginContext {
  /**
   * Generate an object model for type. Beware of recursive types, this method is not able to handle that transparently.
   * Going into recursion (i.e. repeating a type on stack) will throw an exception.
   */
  def objectModel[T](using Type[T]): ObjectModelBuilder[T]

  /** @return New builder for nested serializer */
  def nested[T](using Type[T]): NestedBuilder[T]

  /** @return New builder for nested serializer */
  def nested(using q: Quotes)(t: q.reflect.TypeRepr): NestedBuilder[?]
}
