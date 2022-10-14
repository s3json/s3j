import org.jooq.{Record, SQLDialect, TableField}
import org.jooq.impl.{DSL, SQLDataType, TableImpl}
import org.jooq.tools.jdbc.JDBCUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.ast.JsValue
import s3j.jooq.PgBinaryJsonBinding
import s3j.*
import s3j.io.AstJsonWriter

import java.sql.DriverManager
import scala.jdk.CollectionConverters.*

class PgJsonTest extends AnyFlatSpec with Matchers {
  class TestTable extends TableImpl[Record](DSL.name("test")) {
    val id: TableField[Record, Integer] = createField(DSL.name("id"), SQLDataType.INTEGER)
    val j: TableField[Record, JsValue] = createField(DSL.name("j"), SQLDataType.JSONB, "", new PgBinaryJsonBinding)
  }

  private val testTable = new TestTable

  case class Test(x: Int, y: Boolean) derives JsonFormat

  it should "work for JSON with PostgreSQL" in {
    val conn = DriverManager.getConnection("jdbc:tc:postgresql:13.0:///testdb")
    try {
      val dsl = DSL.using(conn, SQLDialect.POSTGRES)
      def runQuery(sql: String): Unit = conn.prepareStatement(sql).execute()
      def fetchRecord(id: Int): Test =
        dsl.fetchOptional(testTable, testTable.id.eq(id)).get().get(testTable.j).convertTo[Test]

      runQuery(""" CREATE TABLE "test" ("id" SERIAL PRIMARY KEY NOT NULL, "j" JSONB NOT NULL); """)
      runQuery(""" INSERT INTO "test" VALUES (DEFAULT, '{"x":123, "y": true}'::jsonb) """)

      fetchRecord(1) shouldBe Test(123, true)

      dsl.insertInto(testTable)
        .set(testTable.j, Test(999, false).toJsonValue)
        .execute()

      fetchRecord(2) shouldBe Test(999, false)
    } finally JDBCUtils.safeClose(conn)
  }
}
