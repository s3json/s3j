package s3j.macros.generic

import s3j.macros.GenerationContext
import s3j.macros.modifiers.ModifierSet

import java.lang.reflect.InvocationTargetException
import scala.annotation.{experimental, unused}
import scala.collection.mutable

private[macros] trait GenerationStateImpl { this: PluginContextImpl[_] =>
  import q.reflect.*

  // Caching of generated serializers is absolutely necessary:
  // 1) for performance reasons to avoid generating same serializers from scratch
  // 2) for code bloat reasons to reuse equivalent serializers
  // 3) to handle recursive types

  // Caching consists of two layers:
  // 1) core layer comparing type and it's modifiers - cheap layer that could be used right from start
  // 2) plugin layer comparing some plugin-specific ('semantical') identities. These identites could be costly to
  //    generate (they could require parsing of everything just as during generation), but they could lead to much more
  //    precise cache hits
  
  protected class SerializerHandle {
    var order: Int = _
    var variableName: String = _
    var serializedType: TypeRepr = _
    var variableType: TypeRepr = _
    var variableSymbol: Symbol = _
    var variableRef: Term = _
    var definition: Term = _
    var identity: AnyRef = _
    var recursiveWrapper: Boolean = false
  }

  protected sealed trait CachingKey
  protected case class BasicCachingKey(target: TypeRepr, modifiers: ModifierSet) extends CachingKey
  protected case class ImplicitCachingKey(target: TypeRepr) extends CachingKey
  protected case class PluginCachingKey(target: TypeRepr, plugin: String, identity: AnyRef) extends CachingKey

  protected val _serializers: mutable.Map[CachingKey, SerializerHandle] = mutable.HashMap.empty
  protected var _serializerSet: mutable.Set[SerializerHandle] = mutable.HashSet.empty

  def buildFinalBlock(result: () => Symbol): Block = {
    val resultSymbol = result()
    val statements = List.newBuilder[Statement]
    val serializers = _serializerSet.toVector.sortBy(_.order)

    for (s <- serializers if s.recursiveWrapper) {
      val recursiveSym = s.variableType.typeSymbol
      val init = Apply(TypeApply(Select(New(Inferred(s.variableType)), recursiveSym.primaryConstructor),
        List(Inferred(s.serializedType))), List())

      statements += ValDef(s.variableSymbol, Some(init.changeOwner(s.variableSymbol)))
    }

    for (s <- serializers if !s.recursiveWrapper) {
      statements += ValDef(s.variableSymbol, Some(s.definition /* should already have proper owner */))
    }

    for (s <- serializers if s.recursiveWrapper) {
      val setSym = s.variableType.typeSymbol.declaredMethod("set").head
      statements += Apply(Select(s.variableRef, setSym), List(s.definition))
    }

    Block(statements.result(), Typed(Ref(resultSymbol), Inferred(_generatedType)))
  }
}
