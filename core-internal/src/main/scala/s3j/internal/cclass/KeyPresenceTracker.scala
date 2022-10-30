package s3j.internal.cclass

import s3j.internal.utils.Variable

import scala.quoted.{Expr, Quotes, quotes}

class KeyPresenceTracker(keys: Set[String])(using Quotes) {
  private val keyIndices: Map[String, Int] = keys.toVector.sorted.zipWithIndex.toMap
  private val keysPerVariable: Int = 64
  private val keyVarCount: Int = (keyIndices.size + keysPerVariable - 1) / keysPerVariable
  private val keyVariables: IndexedSeq[Variable[Long]] = (0 until keyVarCount).
    map(i => Variable.create("keyMask" + i)(Expr(0L)))

  def variables: Seq[Variable[_]] = keyVariables

  // returns: var -> mask
  private def getVariable(key: String)(using Quotes): (Variable[Long], Expr[Long]) = {
    val keyIndex = keyIndices(key)
    keyVariables(keyIndex / keysPerVariable) -> Expr(1L << (keyIndex % keysPerVariable))
  }

  def markKeyPresence(key: String)(using Quotes): Expr[Unit] = {
    val (keyVar, keyMask) = getVariable(key)
    keyVar := '{ $keyVar | $keyMask }
  }

  def isKeyPresent(key: String)(using Quotes): Expr[Boolean] = {
    val (keyVar, keyMask) = getVariable(key)
    '{ ($keyVar & $keyMask) != 0 }
  }
}
