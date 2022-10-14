package s3j.jooq

import org.jooq.conf.ParamType
import org.jooq.impl.DSL
import s3j.ast.JsValue
import s3j.format.BasicFormats.given
import s3j.io.IoExtensions.*
import org.jooq.*

import java.sql.Types

class PgBinaryJsonBinding extends PgGenericJsonBinding[JSONB] {
  val converter: Converter[JSONB, JsValue] =
    new Converter[JSONB, JsValue] {
      def from(databaseObject: JSONB): JsValue =
        if (databaseObject == null) null
        else databaseObject.data().fromJson[JsValue]

      def to(userObject: JsValue): JSONB =
        if (userObject == null) null
        else JSONB.jsonb(userObject.toJsonString)

      def fromType(): Class[JSONB] = classOf[JSONB]
      def toType: Class[JsValue] = classOf[JsValue]
    }

  protected def typeCast: String = "::jsonb"
  protected def typePlaceholder: String = "?::jsonb"
}
