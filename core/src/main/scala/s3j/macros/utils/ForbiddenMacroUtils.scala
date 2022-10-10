package s3j.macros.utils

import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.util.Property

import java.lang.reflect.Field
import scala.annotation.tailrec
import scala.quoted.runtime.impl.QuotesImpl
import scala.quoted.{Quotes, quotes}

object ForbiddenMacroUtils {
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
    val context = quotes.asInstanceOf[QuotesImpl].ctx
    val cacheKey = getStaticField(QuotesCache.getClass, "QuotesCacheKey")
      .asInstanceOf[Property.Key[scala.collection.mutable.Map[_, _]]]

    context.property(cacheKey).foreach(_.clear())
  }
}
