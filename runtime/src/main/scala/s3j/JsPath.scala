package s3j

import s3j.io.util.EscapeUtils
import scala.collection.mutable

object JsPath {
  case object Root extends JsPath {
    protected def buildString(sb: mutable.StringBuilder): Unit = sb += '$'
  }

  case class Array(parent: JsPath, index: Int) extends JsPath {
    protected def buildString(sb: mutable.StringBuilder): Unit = {
      parent.buildString(sb)
      sb += '['
      sb.append(index)
      sb += ']'
    }
  }

  case class Object(parent: JsPath, key: String) extends JsPath {
    private val keyValid: Boolean = key.nonEmpty
      && Character.isJavaIdentifierStart(key.head)
      && key.tail.forall(Character.isJavaIdentifierPart)

    protected def buildString(sb: mutable.StringBuilder): Unit = {
      parent.buildString(sb)
      if (keyValid) sb += '.' ++= key
      else sb ++= "[\"" ++= EscapeUtils.escape(key) ++= "\"]"
    }
  }
  
  def arr(i: Int): JsPath = JsPath.Array(JsPath.Root, i)
  def obj(k: String): JsPath = JsPath.Object(JsPath.Root, k)
}

sealed trait JsPath {
  def arr(i: Int): JsPath = JsPath.Array(this, i)
  def obj(k: String): JsPath = JsPath.Object(this, k)
  
  protected def buildString(sb: mutable.StringBuilder): Unit

  override def toString: String = {
    val sb = new mutable.StringBuilder()
    buildString(sb)
    sb.result()
  }
}
