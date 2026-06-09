package cosmo0

/** Expression macro provider signature admitted by this compiler-hosted slice.
  */
final case class MacroProviderSignature(
    inputType: String,
    outputType: String,
):
  def isExpressionUntypedToUntyped: Boolean =
    inputType == "Expr[Untyped]" && outputType == "Expr[Untyped]"

  def isDeriveInputToTraitImplementations: Boolean =
    inputType == "DeriveInput" && outputType == "GeneratedTraitImpls"

object MacroProviderSignature:
  val expressionUntypedToUntyped: MacroProviderSignature =
    MacroProviderSignature("Expr[Untyped]", "Expr[Untyped]")

  val deriveInputToTraitImplementations: MacroProviderSignature =
    MacroProviderSignature("DeriveInput", "GeneratedTraitImpls")

/** Provider binding selected by expression macro-call classification. */
sealed trait MacroExpressionProviderBinding:
  def stableKey: String
  def providerPath: String

object MacroExpressionProviderBinding:
  final case class Free(path: String) extends MacroExpressionProviderBinding:
    def stableKey: String = s"free:$path"
    def providerPath: String = path

  final case class Method(receiverType: String, selector: String)
      extends MacroExpressionProviderBinding:
    def stableKey: String = s"method:$receiverType.$selector"
    def providerPath: String = s"$receiverType.$selector"

  final case class Block(path: String) extends MacroExpressionProviderBinding:
    def stableKey: String = s"block:$path"
    def providerPath: String = path

  final case class Template(path: String)
      extends MacroExpressionProviderBinding:
    def stableKey: String = s"template:$path"
    def providerPath: String = path

/** One compiler-hosted provider entry. */
final case class MacroExpressionProviderEntry(
    binding: MacroExpressionProviderBinding,
    provider: CompilerHostedExpressionProvider,
)

/** Result of resolving a provider binding. */
sealed trait MacroProviderLookup

object MacroProviderLookup:
  final case class Found(provider: CompilerHostedExpressionProvider)
      extends MacroProviderLookup
  final case class Disabled(providerIdentity: String)
      extends MacroProviderLookup
  case object Missing extends MacroProviderLookup

/** Registry of compiler-hosted expression macro providers.
  *
  * The registry is intentionally expression-scoped. Compiler-hosted providers
  * are supplied by an embedding layer or by tests; the production compiler does
  * not reserve provider namespaces by default.
  */
final class MacroExpressionProviderRegistry(
    entries: List[MacroExpressionProviderEntry],
    disabledProviderIdentities: Set[String] = Set.empty,
    providerNamespacePrefixes: Set[String] = Set.empty,
):
  private val validation = RegistryValidation.build(
    entries,
    disabledProviderIdentities,
  )

  val diagnostics: List[Diagnostic] =
    validation.diagnostics

  def resolveFree(path: String): MacroProviderLookup =
    resolve(MacroExpressionProviderBinding.Free(path).stableKey)

  def resolveMethod(
      receiverType: String,
      selector: String,
  ): MacroProviderLookup =
    resolve(
      MacroExpressionProviderBinding.Method(receiverType, selector).stableKey,
    )

  def resolveBlock(path: String): MacroProviderLookup =
    resolve(MacroExpressionProviderBinding.Block(path).stableKey)

  def resolveTemplate(path: String): MacroProviderLookup =
    resolve(MacroExpressionProviderBinding.Template(path).stableKey)

  def isReservedProviderPath(path: String): Boolean =
    providerNamespacePrefixes.exists(path.startsWith)

  def hasFreeCandidate(path: String): Boolean =
    validation.available.contains(
      MacroExpressionProviderBinding.Free(path).stableKey,
    ) ||
      validation.disabled.contains(
        MacroExpressionProviderBinding.Free(path).stableKey,
      ) ||
      isReservedProviderPath(path)

  def hasBlockCandidate(path: String): Boolean =
    validation.available.contains(
      MacroExpressionProviderBinding.Block(path).stableKey,
    ) ||
      validation.disabled.contains(
        MacroExpressionProviderBinding.Block(path).stableKey,
      ) ||
      isReservedProviderPath(path)

  def hasTemplateCandidate(path: String): Boolean =
    validation.available.contains(
      MacroExpressionProviderBinding.Template(path).stableKey,
    ) ||
      validation.disabled.contains(
        MacroExpressionProviderBinding.Template(path).stableKey,
      ) ||
      isReservedProviderPath(path)

  private def resolve(key: String): MacroProviderLookup =
    validation.available.get(key) match
      case Some(provider) =>
        MacroProviderLookup.Found(provider)
      case None =>
        validation.disabled.get(key) match
          case Some(providerIdentity) =>
            MacroProviderLookup.Disabled(providerIdentity)
          case None =>
            MacroProviderLookup.Missing

object MacroExpressionProviderRegistry:
  val empty: MacroExpressionProviderRegistry =
    new MacroExpressionProviderRegistry(Nil)

  val default: MacroExpressionProviderRegistry = empty

/** One compiler-hosted derive provider entry. The path is the `@derive(path)`
  * provider key.
  */
final case class MacroDeriveProviderEntry(
    path: String,
    provider: CompilerHostedDeriveProvider,
)

/** Result of resolving a derive provider binding. */
sealed trait MacroDeriveProviderLookup

object MacroDeriveProviderLookup:
  final case class Found(provider: CompilerHostedDeriveProvider)
      extends MacroDeriveProviderLookup
  final case class Disabled(providerIdentity: String)
      extends MacroDeriveProviderLookup
  case object Missing extends MacroDeriveProviderLookup

/** Registry of compiler-hosted derive providers.
  *
  * The registry mirrors expression provider resolution but keeps derive
  * provider signatures and diagnostics scoped to implementation attachments.
  */
final class MacroDeriveProviderRegistry(
    entries: List[MacroDeriveProviderEntry],
    disabledProviderIdentities: Set[String] = Set.empty,
    providerNamespacePrefixes: Set[String] = Set.empty,
):
  private val validation = DeriveRegistryValidation.build(
    entries,
    disabledProviderIdentities,
  )

  val diagnostics: List[Diagnostic] =
    validation.diagnostics

  def resolve(path: String): MacroDeriveProviderLookup =
    validation.available.get(path) match
      case Some(provider) =>
        MacroDeriveProviderLookup.Found(provider)
      case None =>
        validation.disabled.get(path) match
          case Some(providerIdentity) =>
            MacroDeriveProviderLookup.Disabled(providerIdentity)
          case None =>
            MacroDeriveProviderLookup.Missing

  def hasCandidate(path: String): Boolean =
    validation.available.contains(path) ||
      validation.disabled.contains(path) ||
      providerNamespacePrefixes.exists(path.startsWith)

object MacroDeriveProviderRegistry:
  val empty: MacroDeriveProviderRegistry =
    new MacroDeriveProviderRegistry(Nil)

  val default: MacroDeriveProviderRegistry = empty

private final case class DeriveRegistryValidation(
    available: Map[String, CompilerHostedDeriveProvider],
    disabled: Map[String, String],
    diagnostics: List[Diagnostic],
)

private object DeriveRegistryValidation:
  def build(
      entries: List[MacroDeriveProviderEntry],
      disabledProviderIdentities: Set[String],
  ): DeriveRegistryValidation =
    val available =
      scala.collection.mutable.LinkedHashMap.empty[
        String,
        CompilerHostedDeriveProvider,
      ]
    val disabled = scala.collection.mutable.LinkedHashMap.empty[String, String]
    val diagnostics = scala.collection.mutable.ListBuffer.empty[Diagnostic]
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]

    entries.foreach { entry =>
      val key = entry.path
      val provider = entry.provider
      if seen.contains(key) then
        diagnostics += registryDiagnostic(
          "cosmo0.macro.duplicate-provider",
          s"derive macro provider binding $key is registered more than once",
        )
      else
        seen += key
        if disabledProviderIdentities.contains(provider.id) then
          disabled.update(key, provider.id)
          diagnostics += registryDiagnostic(
            "cosmo0.macro.disabled-provider",
            s"derive macro provider ${provider.id} is disabled",
          )
        else if !provider.signature.isDeriveInputToTraitImplementations then
          diagnostics += registryDiagnostic(
            "cosmo0.macro.invalid-provider-signature",
            s"derive macro provider ${provider.id} must have signature @derive def provider(input: DeriveInput): GeneratedTraitImpls",
          )
        else available.update(key, provider)
    }

    DeriveRegistryValidation(
      available.toMap,
      disabled.toMap,
      diagnostics.toList,
    )

  private def registryDiagnostic(code: String, message: String): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      code,
      message,
      None,
    )

private final case class RegistryValidation(
    available: Map[String, CompilerHostedExpressionProvider],
    disabled: Map[String, String],
    diagnostics: List[Diagnostic],
)

private object RegistryValidation:
  def build(
      entries: List[MacroExpressionProviderEntry],
      disabledProviderIdentities: Set[String],
  ): RegistryValidation =
    val available =
      scala.collection.mutable.LinkedHashMap.empty[
        String,
        CompilerHostedExpressionProvider,
      ]
    val disabled = scala.collection.mutable.LinkedHashMap.empty[String, String]
    val diagnostics = scala.collection.mutable.ListBuffer.empty[Diagnostic]
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]

    entries.foreach { entry =>
      val key = entry.binding.stableKey
      val provider = entry.provider
      if seen.contains(key) then
        diagnostics += registryDiagnostic(
          "cosmo0.macro.duplicate-provider",
          s"expression macro provider binding $key is registered more than once",
        )
      else
        seen += key
        if disabledProviderIdentities.contains(provider.id) then
          disabled.update(key, provider.id)
          diagnostics += registryDiagnostic(
            "cosmo0.macro.disabled-provider",
            s"expression macro provider ${provider.id} is disabled",
          )
        else if !provider.signature.isExpressionUntypedToUntyped then
          diagnostics += registryDiagnostic(
            "cosmo0.macro.invalid-provider-signature",
            s"expression macro provider ${provider.id} must have signature @macro def provider(input: Expr[Untyped]): Expr[Untyped]",
          )
        else available.update(key, provider)
    }

    RegistryValidation(
      available.toMap,
      disabled.toMap,
      diagnostics.toList,
    )

  private def registryDiagnostic(code: String, message: String): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      code,
      message,
      None,
    )

/** Compiler-hosted expression provider contract. */
trait CompilerHostedExpressionProvider:
  def id: String
  def signature: MacroProviderSignature =
    MacroProviderSignature.expressionUntypedToUntyped
  def evaluate(input: MacroFunctionInput): MacroFunctionOutput

/** Compiler-hosted derive provider contract. */
trait CompilerHostedDeriveProvider:
  def id: String
  def signature: MacroProviderSignature =
    MacroProviderSignature.deriveInputToTraitImplementations
  def evaluate(input: MacroFunctionInput): MacroFunctionOutput

/** Adapter that keeps compiler-hosted providers on the same serialized
  * input/output boundary that self-hosted providers will use.
  */
object CompilerHostedMacroEvaluator:
  def evaluateExpression(
      provider: CompilerHostedExpressionProvider,
      input: MacroFunctionInput,
  ): MacroFunctionOutput =
    provider.evaluate(input)

  def evaluateDerive(
      provider: CompilerHostedDeriveProvider,
      input: MacroFunctionInput,
  ): MacroFunctionOutput =
    provider.evaluate(input)
