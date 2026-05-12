package cosmo0

enum BackendRequirementKind:
  case Descriptor, RuntimeSymbol, Include, SupportLibrary

final case class BackendRequirement(
    kind: BackendRequirementKind,
    value: String,
):
  require(value.nonEmpty, "backend requirement values must be non-empty")

  def legacyName: String =
    kind match
      case BackendRequirementKind.Descriptor => value
      case BackendRequirementKind.RuntimeSymbol => s"runtime-symbol:$value"
      case BackendRequirementKind.Include => s"include:$value"
      case BackendRequirementKind.SupportLibrary => s"support-library:$value"

object BackendRequirement:
  def descriptor(name: String): BackendRequirement =
    BackendRequirement(BackendRequirementKind.Descriptor, name)

  def runtimeSymbol(symbol: String): BackendRequirement =
    BackendRequirement(BackendRequirementKind.RuntimeSymbol, symbol)

  def include(header: String): BackendRequirement =
    BackendRequirement(BackendRequirementKind.Include, header)

  def supportLibrary(name: String): BackendRequirement =
    BackendRequirement(BackendRequirementKind.SupportLibrary, name)

final case class LirExternBinding(
    abi: String,
    symbol: String,
    requirements: List[BackendRequirement],
):
  require(abi.nonEmpty, "extern binding ABI names must be non-empty")
  require(symbol.nonEmpty, "extern binding symbols must be non-empty")

object TrustedExternAbi:
  val abiName = "cosmo0.extern.v0"

  private final case class TrustedBinding(
      sourceName: String,
      params: List[SourceType],
      returnType: SourceType,
      symbol: String,
      requirements: List[BackendRequirement],
  ):
    def callable(span: SourceSpan): CallableSignature =
      CallableSignature(
        sourceName,
        params.zipWithIndex.map { case (paramType, index) =>
          CallableParam(s"arg$index", paramType, span)
        },
        returnType,
      )

    def lirBinding: LirExternBinding =
      LirExternBinding(abiName, symbol, requirements)

    def accepts(signature: CallableSignature): Boolean =
      params.length == signature.params.length &&
        params.zip(signature.params).forall { case (expected, actual) =>
          SourceType.dealias(expected) == SourceType.Error ||
            SourceType.assignable(actual.valueType, expected)
        } &&
        SourceType.assignable(signature.returnType, returnType)

    def syntheticFunction: LirFunction =
      LirFunction(
        syntheticId(sourceName),
        sourceName,
        params.zipWithIndex.map { case (paramType, index) =>
          LirParam(Lir.localId(s"arg$index"), s"arg$index", Lir.t(paramType))
        },
        Lir.t(returnType),
        locals = Nil,
        blocks = Nil,
        owner = None,
        sourceSignature = None,
        externBinding = Some(lirBinding),
      )

  private val trustedBindings: Map[String, TrustedBinding] =
    List(
      TrustedBinding(
        "print",
        List(SourceType.Error),
        SourceType.Unit,
        "cosmo0_runtime::print",
        List(
          BackendRequirement.runtimeSymbol("cosmo0_runtime::print"),
          BackendRequirement.include("<cstdio>"),
        ),
      ),
      TrustedBinding(
        "println",
        List(SourceType.Error),
        SourceType.Unit,
        "cosmo0_runtime::println",
        List(
          BackendRequirement.runtimeSymbol("cosmo0_runtime::println"),
          BackendRequirement.include("<cstdio>"),
        ),
      ),
      TrustedBinding(
        "read_file",
        List(SourceType.String),
        SourceType.String,
        "cosmo0_runtime::read_file",
        List(
          BackendRequirement.runtimeSymbol("cosmo0_runtime::read_file"),
          BackendRequirement.include("<fstream>"),
        ),
      ),
    ).map(binding => binding.sourceName -> binding).toMap

  def isTrustedSourceName(name: String): Boolean =
    trustedBindings.contains(name)

  def callable(name: String, span: SourceSpan): Option[CallableSignature] =
    trustedBindings.get(name).map(_.callable(span))

  def bindingForDeclaration(function: TypedFunction): Option[LirExternBinding] =
    if function.owner.nonEmpty || function.body.nonEmpty then None
    else
      trustedBindings
        .get(function.name)
        .filter(_.accepts(function.signature))
        .map(_.lirBinding)

  def syntheticFunction(name: String): Option[LirFunction] =
    trustedBindings.get(name).map(_.syntheticFunction)

  def syntheticId(name: String): LirDeclId =
    Lir.declId(s"extern.$name")
