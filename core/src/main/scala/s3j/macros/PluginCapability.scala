package s3j.macros

enum PluginCapability {
  /**
   * Indication that plugin could generate stringy formats. Stringy formats don't make sense for most of the types, and
   * so they require explicit opt-in. Without this capability, request for stringy formats are silently rejected to
   * reduce plugin complexity.
   */
  case SupportsStrings
}
