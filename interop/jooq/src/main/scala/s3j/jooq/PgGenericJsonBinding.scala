package s3j.jooq

import s3j.ast.JsValue
import s3j.format.BasicFormats.given
import s3j.io.IoExtensions.*

import java.sql.Types

import org.jooq.*
import org.jooq.impl.DSL
import org.jooq.conf.ParamType

abstract class PgGenericJsonBinding[T] extends Binding[T, JsValue] {
  protected def toString(obj: JsValue): String =
    if (obj == null) null
    else obj.toJsonString

  protected def fromString(obj: String): JsValue =
    if (obj == null) null
    else obj.fromJson[JsValue]

  protected def typeCast: String
  protected def typePlaceholder: String

  override def sql(ctx: BindingSQLContext[JsValue]): Unit = {
    if (ctx.render().paramType() == ParamType.INLINED) {
      ctx.render().visit(DSL.inline(toString(ctx.value()))).sql(typeCast)
    } else {
      ctx.render().sql(typePlaceholder)
    }
  }

  override def register(ctx: BindingRegisterContext[JsValue]): Unit =
    ctx.statement().registerOutParameter(ctx.index(), Types.VARCHAR)

  override def set(ctx: BindingSetStatementContext[JsValue]): Unit =
    ctx.statement().setString(ctx.index(), toString(ctx.value()))

  override def set(ctx: BindingSetSQLOutputContext[JsValue]): Unit =
    ctx.output().writeString(toString(ctx.value()))

  override def get(ctx: BindingGetResultSetContext[JsValue]): Unit =
    ctx.value(fromString(ctx.resultSet().getString(ctx.index())))

  override def get(ctx: BindingGetStatementContext[JsValue]): Unit =
    ctx.value(fromString(ctx.statement().getString(ctx.index())))

  override def get(ctx: BindingGetSQLInputContext[JsValue]): Unit =
    ctx.value(fromString(ctx.input().readString()))
}
