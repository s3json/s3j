package s3j.macros

enum PluginCapability {
  /**
   * Could generate stringy formats. Without this capability, request for stringy formats are silently rejected
   * to reduce plugin complexity (as most of the plugins don't support strings anyway).
   */
  case SupportsStrings
}
