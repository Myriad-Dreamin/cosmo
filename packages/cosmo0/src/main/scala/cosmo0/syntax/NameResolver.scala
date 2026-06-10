package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Records prefix-first name-resolution facts while elaboration builds the
  * untyped tree.
  *
  * This is not a second AST pass: callers define bindings and resolve paths at
  * the same source sites where untyped nodes are constructed. Type-dependent
  * suffixes such as fields, methods, descriptor operations, and variant payload
  * checks remain obligations for the typer.
  */
final class UntypedNameResolutionBuilder:
  private final class Scope(parent: Option[Scope]):
    private val bindings =
      mutable.LinkedHashMap.empty[String, UntypedBindingFact]

    def parentScope: Option[Scope] = parent

    def define(binding: UntypedBindingFact): Unit =
      bindings.update(binding.name, binding)

    def resolve(name: String): Option[UntypedBindingFact] =
      bindings.get(name).orElse(parent.flatMap(_.resolve(name)))

  private final case class PendingReference(path: UntypedPath)

  private val rootScope = Scope(None)
  private var currentScope = rootScope
  private val bindings = ListBuffer.empty[UntypedBindingFact]
  private val references = ListBuffer.empty[UntypedNameReference]
  private val pendingReferences = ListBuffer.empty[PendingReference]
  private val diagnostics = ListBuffer.empty[Diagnostic]
  private val ordinaryModuleBindings =
    mutable.LinkedHashMap.empty[String, SourceSpan]
  private val foreignAliases =
    mutable.LinkedHashMap.empty[String, SourceCppNamespaceImport]
  private var nextBindingId = 0

  def finish(): UntypedNameResolution =
    UntypedNameResolution(
      bindings.toList,
      references.toList,
      foreignAliases.values.toList,
      diagnostics.toList,
    )

  def enterScope[A](body: => A): A =
    val previous = currentScope
    currentScope = Scope(Some(previous))
    try body
    finally currentScope = previous

  def withParentScope[A](body: => A): A =
    val previous = currentScope
    currentScope = previous.parentScope.getOrElse(previous)
    try body
    finally currentScope = previous

  def noteOrdinaryModuleBinding(name: String, span: SourceSpan): Unit =
    if !ordinaryModuleBindings.contains(name) then
      ordinaryModuleBindings.update(name, span)
    foreignAliases.get(name).foreach { _ =>
      error(
        "cosmo1.name.duplicate-definition",
        s"C++ namespace alias $name conflicts with an existing Cosmo binding",
        span,
      )
    }

  def defineModuleFunction(
      name: String,
      span: SourceSpan,
  ): UntypedBindingFact =
    noteOrdinaryModuleBinding(name, span)
    defineCurrent(
      UntypedBindingKind.Function,
      name,
      span,
      resolvePending = true,
    )

  def defineScopedFunction(
      name: String,
      span: SourceSpan,
  ): UntypedBindingFact =
    defineCurrent(
      UntypedBindingKind.Function,
      name,
      span,
      resolvePending = false,
    )

  def defineModuleClass(
      name: String,
      span: SourceSpan,
  ): UntypedBindingFact =
    noteOrdinaryModuleBinding(name, span)
    defineCurrent(UntypedBindingKind.Class, name, span, resolvePending = true)

  def defineModuleValue(
      name: String,
      span: SourceSpan,
  ): UntypedBindingFact =
    noteOrdinaryModuleBinding(name, span)
    defineCurrent(UntypedBindingKind.Value, name, span, resolvePending = false)

  def defineParameter(name: String, span: SourceSpan): UntypedBindingFact =
    defineCurrent(
      UntypedBindingKind.Parameter,
      name,
      span,
      resolvePending = false,
    )

  def defineLocal(name: String, span: SourceSpan): UntypedBindingFact =
    defineCurrent(UntypedBindingKind.Local, name, span, resolvePending = false)

  def definePattern(name: String, span: SourceSpan): UntypedBindingFact =
    defineCurrent(
      UntypedBindingKind.Pattern,
      name,
      span,
      resolvePending = false,
    )

  def defineCompileTimeIntAlias(
      name: String,
      span: SourceSpan,
  ): UntypedBindingFact =
    defineCurrent(
      UntypedBindingKind.CompileTimeIntAlias,
      name,
      span,
      resolvePending = false,
    )

  def declareForeignAlias(importValue: SourceCppNamespaceImport): Unit =
    ordinaryModuleBindings.get(importValue.alias).foreach { _ =>
      error(
        "cosmo1.name.duplicate-definition",
        s"C++ namespace alias ${importValue.alias} conflicts with an existing Cosmo binding",
        importValue.span,
      )
    }

    foreignAliases.get(importValue.alias) match
      case Some(existing) if existing.namespace == importValue.namespace =>
        val mergedHeaders =
          (existing.headers ::: importValue.headers).distinct
        foreignAliases.update(
          importValue.alias,
          existing.copy(headers = mergedHeaders),
        )
      case Some(existing) =>
        error(
          "cosmo1.name.conflicting-cpp-namespace-alias",
          s"C++ namespace alias ${importValue.alias} already targets ${existing.namespace.cppName}, not ${importValue.namespace.cppName}",
          importValue.span,
        )
      case None =>
        foreignAliases.update(importValue.alias, importValue)
        defineCurrent(
          UntypedBindingKind.ForeignNamespace,
          importValue.alias,
          importValue.span,
          resolvePending = true,
        )

  def resolvePath(path: UntypedPath): Unit =
    path.parts.headOption match
      case Some(root) =>
        currentScope.resolve(root) match
          case Some(binding) =>
            recordReference(path, binding)
          case None =>
            pendingReferences += PendingReference(path)
      case None =>

  private def defineCurrent(
      kind: UntypedBindingKind,
      name: String,
      span: SourceSpan,
      resolvePending: Boolean,
  ): UntypedBindingFact =
    val binding =
      UntypedBindingFact(UntypedBindingId(nextBindingId), kind, name, span)
    nextBindingId += 1
    bindings += binding
    currentScope.define(binding)
    if resolvePending then resolvePendingReferences(name, binding)
    binding

  private def resolvePendingReferences(
      name: String,
      binding: UntypedBindingFact,
  ): Unit =
    pendingReferences.foreach { pending =>
      if pending.path.parts.headOption.contains(name) then
        recordReference(pending.path, binding)
    }

  private def recordReference(
      path: UntypedPath,
      binding: UntypedBindingFact,
  ): Unit =
    if !references.exists(_.path.span == path.span) then
      references += UntypedNameReference(path, binding)

  private def error(
      code: String,
      message: String,
      span: SourceSpan,
  ): Unit =
    diagnostics += Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      code,
      message,
      Some(span),
    )
