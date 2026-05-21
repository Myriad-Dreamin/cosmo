package cosmo0

enum BackendRequirementKind:
  case Descriptor, RuntimeSymbol, Include, SupportLibrary, CppNamespaceImport

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
      case BackendRequirementKind.CppNamespaceImport => s"cpp-namespace-import:$value"

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

  def cppNamespaceImport(value: String): BackendRequirement =
    BackendRequirement(BackendRequirementKind.CppNamespaceImport, value)

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

final case class SourceCppNamespaceImport(
    namespace: CppQualifiedSymbol,
    alias: String,
    headers: List[String],
    span: SourceSpan,
):
  require(alias.nonEmpty, "C++ namespace aliases must be non-empty")
  require(headers.nonEmpty, "C++ namespace imports must name at least one header")
  require(headers.forall(_.startsWith(SourceCppNamespaceImport.headerPrefix)), s"C++ header imports must start with ${SourceCppNamespaceImport.headerPrefix}")

  def canonicalNamespace: String =
    namespace.cppName

  def includeHeaders: List[String] =
    headers.map(header => s"<${header.stripPrefix(SourceCppNamespaceImport.headerPrefix)}>")

  def requirementValue: String =
    s"$alias=${namespace.cppName} from ${headers.mkString(",")}"

object SourceCppNamespaceImport:
  val headerPrefix = "c++/"

  def isCppHeader(value: String): Boolean =
    value.startsWith(headerPrefix) && value.length > headerPrefix.length

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
  private val readDirSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "read_dir")
  private val pathIsFileSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "path_is_file")
  private val pathIsDirSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "path_is_dir")
  private val writeFileSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "write_file")
  private val stringDataSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "string_data")
  private val stringLenSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "string_len")
  private val stringFromBytesSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "string_from_bytes")
  private val commandRunSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "command_run")
  private val jsonParseSymbol =
    CppQualifiedSymbol.global("cosmo0_runtime", "json_parse")
  private val commandType = SourceType.User("Command")
  private val commandResultType = SourceType.User("CommandResult")
  private val commandErrorType = SourceType.User("CommandError")
  private val commandRefType = SourceType.Ref(commandType, mutable = false)
  private val jsonValueType = SourceType.User("JsonValue")
  private val jsonParseErrorType = SourceType.User("JsonParseError")
  private val jsonValueRefType = SourceType.Ref(jsonValueType, mutable = false)
  private val jsonValueOptionType = SourceType.Standard("Option", List(jsonValueType))
  private val boolOptionType = SourceType.Standard("Option", List(SourceType.Bool))
  private val stringOptionType = SourceType.Standard("Option", List(SourceType.String))
  private val usizeOptionType = SourceType.Standard("Option", List(SourceType.Usize))
  private val u8PtrType = SourceType.Standard("Ptr", List(SourceType.Byte))

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
      TrustedBinding(
        "read_dir",
        List(SourceType.String),
        SourceType.Standard("Vec", List(SourceType.String)),
        readDirSymbol,
        List(
          BackendRequirement.runtimeSymbol(readDirSymbol),
          BackendRequirement.include("<filesystem>"),
        ),
      ),
      TrustedBinding(
        "path_is_file",
        List(SourceType.String),
        SourceType.Bool,
        pathIsFileSymbol,
        List(
          BackendRequirement.runtimeSymbol(pathIsFileSymbol),
          BackendRequirement.include("<filesystem>"),
        ),
      ),
      TrustedBinding(
        "path_is_dir",
        List(SourceType.String),
        SourceType.Bool,
        pathIsDirSymbol,
        List(
          BackendRequirement.runtimeSymbol(pathIsDirSymbol),
          BackendRequirement.include("<filesystem>"),
        ),
      ),
      TrustedBinding(
        "write_file",
        List(SourceType.String, SourceType.String),
        SourceType.Unit,
        writeFileSymbol,
        List(
          BackendRequirement.runtimeSymbol(writeFileSymbol),
          BackendRequirement.include("<fstream>"),
        ),
      ),
      TrustedBinding(
        "string_data",
        List(SourceType.String),
        u8PtrType,
        stringDataSymbol,
        List(
          BackendRequirement.runtimeSymbol(stringDataSymbol),
          BackendRequirement.include("<string>"),
        ),
      ),
      TrustedBinding(
        "string_len",
        List(SourceType.String),
        SourceType.Usize,
        stringLenSymbol,
        List(
          BackendRequirement.runtimeSymbol(stringLenSymbol),
          BackendRequirement.include("<string>"),
        ),
      ),
      TrustedBinding(
        "string_from_bytes",
        List(u8PtrType, SourceType.Usize),
        SourceType.String,
        stringFromBytesSymbol,
        List(
          BackendRequirement.runtimeSymbol(stringFromBytesSymbol),
          BackendRequirement.include("<string>"),
        ),
      ),
      TrustedBinding(
        "json_parse",
        List(SourceType.String),
        SourceType.Standard("Result", List(jsonValueType, jsonParseErrorType)),
        jsonParseSymbol,
        List(
          BackendRequirement.runtimeSymbol(jsonParseSymbol),
          BackendRequirement.include("<nlohmann/json.hpp>"),
          BackendRequirement.include("<string>"),
        ),
      ),
      TrustedBinding(
        "command_run",
        List(commandRefType),
        SourceType.Standard("Result", List(commandResultType, commandErrorType)),
        commandRunSymbol,
        List(
          BackendRequirement.runtimeSymbol(commandRunSymbol),
          BackendRequirement.include("<cstdio>"),
          BackendRequirement.include("<cstdlib>"),
          BackendRequirement.include("<string>"),
          BackendRequirement.include("<sys/wait.h>"),
          BackendRequirement.include("<vector>"),
        ),
      ),
      jsonBinding("json_is_null", List(jsonValueRefType), SourceType.Bool),
      jsonBinding("json_as_bool", List(jsonValueRefType), boolOptionType),
      jsonBinding("json_as_number_text", List(jsonValueRefType), stringOptionType),
      jsonBinding("json_as_string", List(jsonValueRefType), stringOptionType),
      jsonBinding("json_array_len", List(jsonValueRefType), usizeOptionType),
      jsonBinding("json_array_get", List(jsonValueRefType, SourceType.Usize), jsonValueOptionType),
      jsonBinding("json_field", List(jsonValueRefType, SourceType.String), jsonValueOptionType),
    ).map(binding => binding.sourceName -> binding).toMap

  private def jsonBinding(
      name: String,
      params: List[SourceType],
      returnType: SourceType,
  ): TrustedBinding =
    val symbol = CppQualifiedSymbol.global("cosmo0_runtime", name)
    TrustedBinding(
      name,
      params,
      returnType,
      symbol,
      List(
        BackendRequirement.runtimeSymbol(symbol),
        BackendRequirement.include("<nlohmann/json.hpp>"),
        BackendRequirement.include("<string>"),
      ),
    )

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
