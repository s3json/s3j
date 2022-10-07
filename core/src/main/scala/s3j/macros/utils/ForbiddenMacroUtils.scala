package s3j.macros.utils

import java.lang.reflect.Field
import scala.annotation.tailrec
import scala.quoted.{Quotes, quotes}

object ForbiddenMacroUtils {
  @tailrec
  private def findDeclaredField(cls: Class[_], fieldName: String): Field =
    try cls.getDeclaredField(fieldName)
    catch {
      case e: NoSuchFieldException if cls != classOf[Object] =>
        findDeclaredField(cls.getSuperclass, fieldName)
    }

  private def getField(instance: AnyRef, fieldName: String): AnyRef = {
    val f = findDeclaredField(instance.getClass, fieldName)
    f.setAccessible(true)
    f.get(instance)
  }

  private def getStaticField(cls: Class[_], fieldName: String): AnyRef = {
    val f = cls.getDeclaredField(fieldName)
    f.setAccessible(true)
    f.get(null)
  }

  /**
   * Workaround for dotty bug #16147
   *
   * @see https://github.com/lampepfl/dotty/issues/16147
   */
  def clearQuotesCache()(using Quotes): Unit = {
    val context = getField(quotes, "ctx")
    val properties = getField(context, "_moreProperties").asInstanceOf[Map[Any, _]]
    val cacheKey = getStaticField(Class.forName("dotty.tools.dotc.quoted.QuotesCache$"), "QuotesCacheKey")

    val quotesCache = properties(cacheKey).asInstanceOf[scala.collection.mutable.Map[_, _]]
    quotesCache.clear()
  }
}
