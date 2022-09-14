package s3j.annotations.naming

enum CaseConvention {
  case CamelCase            // camelCase
  case SnakeCase            // snake_case
  case ScreamingSnakeCase   // SCREAMING_SNAKE_CASE
  case KebabCase            // kebab-case
  case DotnetCase           // Dotnet-Case (e.g. as in powershell)
  case PascalCase           // PascalCase
}
