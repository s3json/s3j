package s3j.macros.traits

import s3j.macros.codegen.Position
import s3j.macros.traits.ReportingBuilder.StackEntry
import s3j.macros.utils.GenerationPath

import scala.quoted.Type

object ReportingBuilder {
  case class StackEntry(t: Type[?], label: Option[String], path: Seq[GenerationPath])
}

trait ReportingBuilder {
  /** Override currently active plugin (when delegating) */
  def usePlugin(className: String): this.type

  /** Add entries to generation stack */
  def stackEntries(e: StackEntry*): this.type
  
  /** Use specified position as default */
  def defaultPosition(pos: Option[Position]): this.type

  /** @return Built [[ErrorReporting]] instance */
  def build(): ErrorReporting
}
