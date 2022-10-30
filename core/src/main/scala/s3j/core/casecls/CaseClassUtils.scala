package s3j.core.casecls

import s3j.core.casecls.CaseClassContext.ObjectField
import s3j.core.casecls.impl.{CaseClassCodeGenerator, CaseClassSchemaGenerator, ObjectFields}
import s3j.macros.generic.{Extensions, GenerationMode}
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
}
