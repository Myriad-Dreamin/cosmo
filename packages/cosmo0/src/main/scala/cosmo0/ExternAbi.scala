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

  def runtimeSymbol(symbol: CppQualifiedSymbol): BackendRequirement =
    runtimeSymbol(symbol.canonical)

  def include(header: String): BackendRequirement =
    BackendRequirement(BackendRequirementKind.Include, header)

  def supportLibrary(name: String): BackendRequirement =
    BackendRequirement(BackendRequirementKind.SupportLibrary, name)

final case class SourceExternBinding(
    abi: String,
    name: Option[String],
    supportLibrary: Option[String],
    span: SourceSpan,
):
  require(abi.nonEmpty, "source extern ABI names must be non-empty")

final case class SourceCInclude(
    header: String,
    span: SourceSpan,
):
  require(header.nonEmpty, "C include headers must be non-empty")

final case class CppQualifiedSymbol(
    parts: List[String],
    absolute: Boolean = true,
):
  require(parts.nonEmpty, "C++ qualified symbols must have at least one part")
  require(
    parts.forall(CppQualifiedSymbol.isIdentifier),
    s"C++ qualified symbol parts must be identifiers: ${parts.mkString("::")}",
  )

  def namespace: List[String] =
    parts.dropRight(1)

  def name: String =
    parts.last

  def canonical: String =
    parts.mkString("::")

  def cppName: String =
    if absolute then s"::$canonical" else canonical

  override def toString: String =
    canonical

object CppQualifiedSymbol:
  private val Identifier =
    raw"[A-Za-z_][A-Za-z0-9_]*".r

  def global(parts: String*): CppQualifiedSymbol =
    CppQualifiedSymbol(parts.toList, absolute = true)

  def relative(parts: String*): CppQualifiedSymbol =
    CppQualifiedSymbol(parts.toList, absolute = false)

  def parse(value: String, absoluteByDefault: Boolean = true): CppQualifiedSymbol =
    require(value.nonEmpty, "C++ qualified symbols must be non-empty")
    val absolute = value.startsWith("::") || absoluteByDefault
    val trimmed =
      if value.startsWith("::") then value.drop(2) else value
    CppQualifiedSymbol(trimmed.split("::", -1).toList, absolute)

  def isIdentifier(value: String): Boolean =
    Identifier.pattern.matcher(value).matches

final case class LirExternBinding(
    abi: String,
    symbol: CppQualifiedSymbol,
    requirements: List[BackendRequirement],
):
  require(abi.nonEmpty, "extern binding ABI names must be non-empty")

object TrustedExternAbi:
  val abiName = "cosmo0.extern.v0"
  val directCAbiName = "c"

  private val printSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "print")
  private val printlnSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "println")
  private val readFileSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "read_file")

  private final case class TrustedBinding(
      sourceName: String,
      params: List[SourceType],
      returnType: SourceType,
      symbol: CppQualifiedSymbol,
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
        printSymbol,
        List(
          BackendRequirement.runtimeSymbol(printSymbol),
          BackendRequirement.include("<cstdio>"),
        ),
      ),
      TrustedBinding(
        "println",
        List(SourceType.Error),
        SourceType.Unit,
        printlnSymbol,
        List(
          BackendRequirement.runtimeSymbol(printlnSymbol),
          BackendRequirement.include("<cstdio>"),
        ),
      ),
      TrustedBinding(
        "read_file",
        List(SourceType.String),
        SourceType.String,
        readFileSymbol,
        List(
          BackendRequirement.runtimeSymbol(readFileSymbol),
          BackendRequirement.include("<fstream>"),
        ),
      ),
    ).map(binding => binding.sourceName -> binding).toMap

  def isTrustedSourceName(name: String): Boolean =
    trustedBindings.contains(name)

  def callable(name: String, span: SourceSpan): Option[CallableSignature] =
    trustedBindings.get(name).map(_.callable(span))

  def isSupportedAbiName(name: String): Boolean =
    name == abiName || name == directCAbiName

  def bindingForDeclaration(function: TypedFunction): Option[LirExternBinding] =
    function.externBinding.flatMap(directCBindingForDeclaration(function, _)).orElse {
      if function.owner.nonEmpty || function.body.nonEmpty then None
      else
        trustedBindings
          .get(function.name)
          .filter(_.accepts(function.signature))
          .map(_.lirBinding)
    }

  private def directCBindingForDeclaration(
      function: TypedFunction,
      sourceBinding: SourceExternBinding,
  ): Option[LirExternBinding] =
    if function.owner.nonEmpty || function.body.nonEmpty || sourceBinding.abi != directCAbiName then None
    else
      val symbolName = sourceBinding.name.getOrElse(function.name)
      if !CppQualifiedSymbol.isIdentifier(symbolName) then None
      else
        val symbol = CppQualifiedSymbol.relative(symbolName)
        Some(
          LirExternBinding(
            directCAbiName,
            symbol,
            List(BackendRequirement.runtimeSymbol(symbol)) ++
              sourceBinding.supportLibrary.map(BackendRequirement.supportLibrary),
          ),
        )

  def syntheticFunction(name: String): Option[LirFunction] =
    trustedBindings.get(name).map(_.syntheticFunction)

  def syntheticId(name: String): LirDeclId =
    Lir.declId(s"extern.$name")
