package s3j.macros.codegen

/** Class for producing unique variable names */
trait NameGenerator {
  /** @return Unique variable name */
  def freshName(): String
  
  /** @return Unique variable name with a prefix for debugging */
  def freshName(prefix: String): String
}
