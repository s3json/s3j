package s3j.jooq

import org.jooq.*
import org.jooq.conf.ParamType
import org.jooq.impl.DSL
import s3j.ast.JsValue
import s3j.format.BasicFormats.given
import s3j.io.IoExtensions.*

import java.sql.Types

class PgJsonBinding extends PgGenericJsonBinding[JSON] {
  val converter: Converter[JSON, JsValue] =
    new Converter[JSON, JsValue] {
      def from(databaseObject: JSON): JsValue =
        if (databaseObject == null) null
        else databaseObject.data().fromJson[JsValue]

      def to(userObject: JsValue): JSON =
        if (userObject == null) null
        else JSON.json(userObject.toJsonString)

      def fromType(): Class[JSON] = classOf[JSON]
      def toType: Class[JsValue] = classOf[JsValue]
    }

  protected def typeCast: String = "::json"
  protected def typePlaceholder: String = "?::json"
}
