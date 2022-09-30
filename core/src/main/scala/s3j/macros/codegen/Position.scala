package s3j.macros.codegen

import java.nio.file.Path
import scala.quoted.Quotes

object Position {
  import s3j.macros.codegen.Position as PositionWrapper

  /** Implicit conversion to conveniently wrap [[Quotes.reflect.Position]] instances. */
  given wrapPosition(using q: Quotes): Conversion[q.reflect.Position, Position] = Position.apply _

  /** Wrap a Position from quotes universe into path-independent position */
  def apply(using q: Quotes)(pos: q.reflect.Position): Position = {
    import q.reflect.*
    new PositionWrapper(pos)
  }
}

/** Path-independent version of position class to pass it around more easily. */
class Position private(
  private val underlying: AnyRef
  // Any extracted fields here? (e.g. filename, content, offsets?)
) {
  /** @return Unwrapped position inside of [[Quotes]] universe */
  def toPos(using q: Quotes): q.reflect.Position = underlying.asInstanceOf[q.reflect.Position]

  override def toString: String = underlying.toString
}
