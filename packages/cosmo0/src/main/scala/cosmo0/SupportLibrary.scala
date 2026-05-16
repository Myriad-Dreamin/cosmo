package cosmo0

final case class SupportLibraryId private (value: String):
  def requirement: BackendRequirement =
    BackendRequirement.supportLibrary(value)

  def rustLibraryTarget: String =
    s"cosmo_${value.replace('-', '_')}"

  def cSymbolPrefix: String =
    s"${rustLibraryTarget}_"

  def crateDirectory: String =
    s"${SupportLibraryPipeline.cratesRoot}/$value"

  override def toString: String = value

object SupportLibraryId:
  private val Pattern =
    raw"[a-z][a-z0-9]*(?:-[a-z0-9]+)*".r

  def parse(value: String): Either[String, SupportLibraryId] =
    if value.isEmpty then
      Left("support-library id must be non-empty")
    else if value.exists(ch => ch == '\n' || ch == '\r') then
      Left("support-library id must be a single line")
    else if Pattern.pattern.matcher(value).matches then
      Right(new SupportLibraryId(value))
    else
      Left("support-library id must match [a-z][a-z0-9]*(?:-[a-z0-9]+)*")

  def validate(value: String): Option[String] =
    parse(value) match
      case Left(message) => Some(message)
      case Right(_)      => None

  def unsafe(value: String): SupportLibraryId =
    parse(value) match
      case Right(id)     => id
      case Left(message) => throw new IllegalArgumentException(message)

enum SupportLibraryArtifactKind:
  case Static, Shared

enum SupportLibraryPlatform:
  case Linux, Macos, Windows

  def staticLibraryName(rustLibraryTarget: String): String =
    this match
      case Windows => s"$rustLibraryTarget.lib"
      case _       => s"lib$rustLibraryTarget.a"

  def sharedLibraryName(rustLibraryTarget: String): String =
    this match
      case Linux   => s"lib$rustLibraryTarget.so"
      case Macos   => s"lib$rustLibraryTarget.dylib"
      case Windows => s"$rustLibraryTarget.dll"

final case class SupportLibraryArtifact(
    id: SupportLibraryId,
    kind: SupportLibraryArtifactKind = SupportLibraryArtifactKind.Static,
    profile: String = SupportLibraryPipeline.defaultProfile,
    platform: SupportLibraryPlatform = SupportLibraryPlatform.Linux,
):
  require(profile.nonEmpty, "support-library build profile must be non-empty")

  def fileName: String =
    kind match
      case SupportLibraryArtifactKind.Static => platform.staticLibraryName(id.rustLibraryTarget)
      case SupportLibraryArtifactKind.Shared => platform.sharedLibraryName(id.rustLibraryTarget)

  def directory: String =
    s"${SupportLibraryPipeline.artifactRoot}/$profile/${id.value}"

  def path: String =
    s"$directory/$fileName"

final case class SupportLibraryLinkItem(
    id: SupportLibraryId,
    artifact: SupportLibraryArtifact,
):
  def linkArgument: String =
    artifact.path

final case class SupportLibraryAvailableArtifact(
    id: SupportLibraryId,
    path: String,
    abiVersion: Int,
):
  require(path.nonEmpty, "support-library artifact paths must be non-empty")

final case class SupportLibraryLinkPlan(
    items: List[SupportLibraryLinkItem],
):
  def linkArguments: List[String] =
    items.map(_.linkArgument)

  def validateArtifacts(available: List[SupportLibraryAvailableArtifact]): List[Diagnostic] =
    val byId = available.map(artifact => artifact.id -> artifact).toMap
    val missing = items.flatMap { item =>
      if byId.contains(item.id) then None
      else
        Some(
          SupportLibraryPipeline.diagnostic(
            "cosmo0.support-library.missing-artifact",
            s"support-library ${item.id.value} is required but no artifact was provided at ${item.artifact.path}",
          ),
        )
    }
    val incompatible = items.flatMap { item =>
      byId.get(item.id).filter(_.abiVersion != SupportLibraryPipeline.abiVersion).map { artifact =>
        SupportLibraryPipeline.diagnostic(
          "cosmo0.support-library.incompatible-artifact",
          s"support-library ${item.id.value} artifact ${artifact.path} uses ABI ${artifact.abiVersion}; expected ABI ${SupportLibraryPipeline.abiVersion}",
        )
      }
    }
    missing ::: incompatible

object SupportLibraryLinkPlan:
  val empty: SupportLibraryLinkPlan =
    SupportLibraryLinkPlan(Nil)

  def fromBackendRequirements(
      requirements: List[BackendRequirement],
      profile: String = SupportLibraryPipeline.defaultProfile,
      platform: SupportLibraryPlatform = SupportLibraryPlatform.Linux,
  ): Either[List[Diagnostic], SupportLibraryLinkPlan] =
    val ids = supportLibraryRequirementValues(requirements)
    val parsed = ids.map(value => value -> SupportLibraryId.parse(value))
    val diagnostics = parsed.collect { case (value, Left(message)) =>
      SupportLibraryPipeline.diagnostic(
        "cosmo0.support-library.invalid-id",
        s"invalid support-library requirement $value: $message",
      )
    }

    if diagnostics.nonEmpty then Left(diagnostics)
    else
      val items = parsed.collect { case (_, Right(id)) =>
        SupportLibraryLinkItem(id, SupportLibraryArtifact(id, SupportLibraryArtifactKind.Static, profile, platform))
      }
      Right(SupportLibraryLinkPlan(items))

  private def supportLibraryRequirementValues(requirements: List[BackendRequirement]): List[String] =
    requirements
      .collect { case BackendRequirement(BackendRequirementKind.SupportLibrary, value) => value }
      .distinct
      .sorted

object SupportLibraryPipeline:
  val abiVersion: Int = 1
  val cratesRoot: String = "crates"
  val artifactRoot: String = "target/cosmo/support-libraries"
  val defaultProfile: String = "release"
  val smokeSupportLibrary: SupportLibraryId =
    SupportLibraryId.unsafe("support-smoke")

  def diagnostic(code: String, message: String): Diagnostic =
    Diagnostic(
      Phase.Compile,
      DiagnosticSeverity.Error,
      code,
      message,
    )
