package s3j.core.casecls

import s3j.core.casecls.CaseClassContext.ObjectField
import s3j.core.casecls.impl.{CaseClassCodeGenerator, CaseClassSchemaGenerator, ObjectFields}
import s3j.macros.generic.{Extensions, GenerationMode}
import s3j.macros.modifiers.ModifierSet
import s3j.macros.schema.SchemaExpr

import scala.quoted.{Expr, Quotes, Type}

object CaseClassUtils {
  /**
   * Convert object field specification into actual serializer code
   *
   * @param field Field model
   * @return      Generated serializer code (format, encoder or decoder depending on mode)
   */
  def generateCode[T](field: ObjectField[T])(using Quotes, Type[T]): CaseClassCode[T] =
    new CaseClassCodeGenerator[T](field).result

  /** Convert object field specification into schema expression */
  def generateSchema[T](field: ObjectField[T])(using Quotes, Type[T]): SchemaExpr[T] =
    new CaseClassSchemaGenerator[T](field).result

  /**
   * Create version of object field which allows (and discards) unknown keys.
   *
   * @param field Field to use
   * @return      Version of field which discards unknown keys
   */
  def allowUnknownKeys[T](field: ObjectField[T]): ObjectField[T] =
    if (field.handlesDynamicKeys) field
    else new ObjectFields.AllowUnknownKeysField[T](field)

  /**
   * Create expression with constructor invocation roughly equivalent to `new T[...typeArgs](...args)`.
   *
   * @param args     Constructor arguments. When constructor has multiple parameter lists, arguments are reshaped
   *                 automatically. Flattened arguments should be supplied in this case.
   * @param typeArgs Type arguments, if target type requires them.
   * @return         Constructor invocation expression
   */
  def createInstance[T](args: Seq[Expr[?]], typeArgs: Seq[Type[?]] = Nil)(using q: Quotes, tt: Type[T]): Expr[T] = {
    import q.reflect.*

    val tpe: TypeRepr = TypeRepr.of[T]
    val tpeSym: Symbol = tpe.typeSymbol

    val ctor: Symbol = tpeSym.primaryConstructor
    val typeParams: Vector[Symbol] = ctor.paramSymss.flatten.filter(_.isTypeParam).toVector
    val paramLists: List[List[Symbol]] = ctor.paramSymss.filterNot(_.forall(_.isTypeParam))

    if (typeParams.size != typeArgs.size) {
      throw new IllegalArgumentException("Formal and actual type parameter lists differ in length for " +
        s"type ${Type.show[T]}: formal=${typeParams.size}, actual=${typeArgs.size}")
    }

    val nParams = paramLists.map(_.size).sum
    if (nParams != args.size) {
      throw new IllegalArgumentException("Formal and actual parameter lists differ in length for " +
        s"type ${Type.show[T]}: formal=$nParams, actual=${args.size}")
    }

    val resultType: TypeTree =
      if (typeArgs.nonEmpty) Applied(TypeIdent(tpeSym), typeArgs.map(t => Inferred(TypeRepr.of(using t))).toList)
      else TypeIdent(tpeSym)


    val listLengths: Array[Int] = paramLists.map(_.size).toArray
    val listOffsets: Array[Int] = new Array[Int](listLengths.length)
    for (i <- 1 until listLengths.length) {
      listOffsets(i) = listOffsets(i - 1) + listLengths(i - 1)
    }

    var result: Term = Select(New(resultType), ctor)
    if (typeArgs.nonEmpty) {
      result = TypeApply(result, typeArgs.map(t => TypeTree.of(using t)).toList)
    }

    paramLists.indices
      .map(i => args.slice(listOffsets(i), listOffsets(i) + listLengths(i)).map(_.asTerm).toList)
      .foldLeft(result)((t, args) => Apply(t, args))
      .asExprOf[T]
  }
}
