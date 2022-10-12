package s3j.macros.generic

import s3j.macros.PluginContext.SymbolModifiers
import s3j.macros.modifiers.ModifierParser.{AnnotationModifier, TextModifier}

import scala.collection.mutable
import s3j.macros.modifiers.{Modifier, ModifierContext, ModifierKey, ModifierSet}
import s3j.macros.utils.{MacroUtils, QualifiedName, ReportingUtils}

import scala.annotation.{Annotation, targetName}
import scala.quoted.{Expr, Quotes, Type}
import scala.quoted.runtime.StopMacroExpansion
import scala.util.control.NonFatal

private[macros] transparent trait ModifierParserImpl { this: PluginContextImpl[_] =>
  import q.reflect.*

  // == State: =========================================================================================================

  private lazy val _rootModifiers: ModifierSet = parseRootModifiers()
  private val _modifierCache: mutable.Map[Symbol, ModifierData] = mutable.HashMap.empty
  private val _seenAnnotations: mutable.Set[String] = mutable.HashSet.empty
  private val _unparsedAnnotations: mutable.Set[String] = mutable.HashSet.empty

  protected case class AnnotationData(pos: Position, annotation: TypeRepr, typeArgs: List[TypeRepr], 
                                      args: List[Term]) 
  { ann =>
    val symbol: Symbol = annotation.typeSymbol
    val fullName: String = symbol.fullName

    def toModifier(ctx: ModifierContext): AnnotationModifier =
      new AnnotationModifier {
        val annotation: Type[_ <: Annotation] = ann.annotation.asType.asInstanceOf[Type[_ <: Annotation]]
        val typeArgs: List[Type[_]] = ann.typeArgs.map(_.asType)
        val args: List[Expr[Any]] = ann.args.map(_.asExpr)
        val typeName: String = ann.symbol.fullName
        val context: ModifierContext = ctx

        override def toString: String =
          s"AnnotationModifier($ann, context=$ctx)"
      }

    override def toString: String = PluginCtxUtils.formatAnnotation(annotation, typeArgs, args)
  }

  protected case class TextModifierImpl(context: ModifierContext, typeName: String, content: String)
  extends TextModifier {
    override def toString: String =
      if (content.nonEmpty) s"@$typeName=$content"
      else s"@$typeName"
  }

  private case class ModifierData(
    symbol: Symbol, parent: Option[ModifierData], own: ModifierSet,
    inherited: ModifierSet
  ) extends SymbolModifiers {
    override def toString: String =
      s"SymbolModifiers(symbol='${symbol.fullName}', own=$own, inherited=$inherited)"
  }

  /** Split annotation application tree into annotation type and application arguments */
  protected def parseAnnotation(annot: Term): AnnotationData = annot match {
    case Apply(Select(New(tpe), "<init>"), args) => AnnotationData(annot.pos, tpe.tpe, Nil, args)
    case Apply(TypeApply(Select(New(Applied(tpe, typeArgTrees)), "<init>"), _), args) =>
      val typeArgs = typeArgTrees.map {
        case tt: TypeTree => tt.tpe
        case other => report.errorAndAbort("Could not parse annotation term: " + annot.show(using Printer.TreeStructure)
          + ": could not parse type: " + other.show(using Printer.TreeStructure))
      }

      AnnotationData(annot.pos, tpe.tpe, typeArgs, args)

    case Apply(TypeApply(Select(New(t), "<init>"), _), args) =>
      AnnotationData(annot.pos, t.tpe, Nil, args)

    case _ => report.errorAndAbort("Could not parse annotation term: " + annot
      .show(using Printer.TreeStructure), annot.pos)
  }

  private def parseRootModifiers(): ModifierSet = {
    val modifiers = MacroUtils
      .macroSettings(MacroUtils.ModifierPrefix)
      .map { setting =>
        setting.split("=", 2) match {
          case Array(className) => TextModifierImpl(ModifierContext.Generic, className, "")
          case Array(className, data) => TextModifierImpl(ModifierContext.Generic, className, data)
        }
      }
      .map { modifier =>
        _modifiers.get(modifier.typeName) match {
          case Some(parser) => 
            try parser.modifierParser(modifier)
            catch {
              case NonFatal(e) => report.errorAndAbort(ReportingUtils.formatException(
                "Failed to parse a global modifier: " + modifier, e))
            }
            
          case None => report.errorAndAbort("No plugin is available to parse a global modifier " + modifier + ". " +
            "Consider adding an 'usePlugin' global setting to load appropriate plugin.")
        }
      }

    ModifierSet(modifiers:_*)
  }

  /** @return true if annotation was recognized as meta-annotation */
  private def processMetaAnnotation(ann: AnnotationData): Boolean =
    ann.fullName match {
      case "s3j.annotations.usePlugin" =>
        if (ann.typeArgs.length != 1) {
          report.errorAndAbort("@usePlugin must have exactly one type argument", ann.pos)
        }

        val pluginName = ann.typeArgs.head.typeSymbol.fullName
        try loadPlugin(pluginName)
        catch {
          case e: StopMacroExpansion => throw e
          case NonFatal(e) => ReportingUtils.reportException("Failed to load plugin " + pluginName, e, Some(ann.pos))
        }

        true

      case _ => false
    }

  protected def checkRegisteredModifiers(className: String, modifiers: Set[String]): Unit = {
    if (!modifiers.exists(_unparsedAnnotations)) {
      return
    }

    val sb = new mutable.StringBuilder()
    sb ++= "Plugin '" ++= className ++= "' registered a modifier parser for annotation types which were already seen "
    sb ++= "and skipped. Result is very likely to be inconsistent. Consider adding '@usePlugin' to these annotations "
    sb ++= "to enforce proper plugin loading order.\n\n"

    sb ++= "Annotation types:\n"
    for (a <- modifiers if _unparsedAnnotations(a)) sb ++= " - " ++= a ++= "\n"

    report.errorAndAbort(sb.result())
  }

  def symbolModifiers(using q: Quotes)(sym: q.reflect.Symbol): SymbolModifiers =
    symbolModifiers0(sym.asInstanceOf[Symbol])

  protected def symbolModifiers0(startSymbol: Symbol): SymbolModifiers = {
    var parents: List[Symbol] = startSymbol :: Nil

    var currentParent: Symbol = startSymbol.maybeOwner
    while (currentParent != Symbol.noSymbol && !_modifierCache.contains(currentParent)) {
      parents = currentParent :: parents
      currentParent = currentParent.maybeOwner
    }

    while (parents.nonEmpty) {
      val sym = parents.head
      parents = parents.tail

      val parentModifiers: Option[ModifierData] =
        Some(sym.maybeOwner).filter(_ != Symbol.noSymbol).map(_modifierCache)

      val annotations: Set[AnnotationData] =
        sym.annotations.map(parseAnnotation).filterNot(processMetaAnnotation).toSet

      for (a <- annotations if !_seenAnnotations.contains(a.fullName)) {
        for (ma <- a.symbol.annotations) processMetaAnnotation(parseAnnotation(ma))
        _seenAnnotations.add(a.fullName)
      }

      val (parsed, unparsed) = annotations.partition(a => _modifiers.contains(a.fullName))
      _unparsedAnnotations.addAll(unparsed.map(_.fullName))

      val context: ModifierContext = // TODO
        if (sym.flags.is(Flags.Enum)) ModifierContext.Enum
        else if (sym.isClassDef && sym.flags.is(Flags.Trait) && sym.flags.is(Flags.Sealed)) ModifierContext.Enum
        else ModifierContext.Generic

      val modifiers: Seq[Modifier] = parsed
        .map { ann =>
          val handler = _modifiers(ann.fullName)
          val modifier: Modifier =
            try handler.modifierParser(ann.toModifier(context))
            catch { case NonFatal(e) => ReportingUtils.reportException(s"Plugin ${handler.className} failed to parse " +
              s"annotation $ann", e, Some(ann.pos)) }

          (ann, modifier)
        }
        .groupMap(_._2.key)(identity)
        .collect {
          case (_, vs) if vs.size == 1 => vs.head._2
          case (k, vs) =>
            val sb = new mutable.StringBuilder()
            sb ++= sym.toString ++= " has conflicting annotations for modifier '" ++= k.toString ++= "':\n\n"

            for (v <- vs) sb ++= " - " ++= v._1.toString ++= "\n"

            ReportingUtils.reportError(sb.result(), sym.pos)
        }
        .toVector

      val own = ModifierSet(modifiers:_*)
      val inherited = ModifierSet.inherit(parentModifiers.fold(_rootModifiers)(_.inherited), own)
      
      val ret = ModifierData(sym, parentModifiers, own, inherited)

      _modifierCache.put(sym, ret)
    }

    _modifierCache(startSymbol)
  }
}
