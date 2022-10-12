package s3j.macros.codegen

import scala.quoted.{Quotes, Type, quotes}

object PluginUtils {
  /** @return Whether types `A` and `B` are the same */
  def typeEquals[A, B](using Quotes, Type[A], Type[B]): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A] =:= TypeRepr.of[B]
  }
}
