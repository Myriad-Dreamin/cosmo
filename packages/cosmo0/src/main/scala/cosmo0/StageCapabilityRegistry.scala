package cosmo0

final case class StageCapabilityProfile(
    name: String,
    requiredPrimitiveDescriptors: Set[String],
    requiredStdCapabilities: Set[String],
    requiredBackendRequirements: Set[BackendRequirement] = Set.empty,
)

final case class StageCapabilityAvailability(
    primitiveDescriptors: Set[String],
    stdCapabilities: Set[String],
    backendRequirements: Set[BackendRequirement],
)

object StageCapabilityRegistry:
  val Cosmo1Stage1 = "cosmo1.stage1"

  val Core0Stage = "core0.stage"
  val Core0Text = "core0.text"
  val Core0TextOutput = "core0.text-output"
  val Core0OptionResultVec = "core0.option-result-vec"
  val Core0PathFs = "core0.path-fs"
  val Core0CharClass = "core0.char-class"

  val Core0Json = "core0.json"
  val Core0Command = "core0.command"
  val Core0ArenaId = "core0.arena-id"
  val Core0MapSet = "core0.map-set"
  val Core0BigNumber = "core0.big-number"

  val laterStageStdCapabilities: Set[String] =
    Set(
      Core0Json,
      Core0Command,
      Core0ArenaId,
      Core0MapSet,
      Core0BigNumber,
    )

  private val stage1PrimitiveDescriptors: Set[String] =
    Set(
      "Unit",
      "Bool",
      "Char",
      "String",
      "i32",
      "u8",
      "usize",
    )

  val stage1Profile: StageCapabilityProfile =
    StageCapabilityProfile(
      Cosmo1Stage1,
      requiredPrimitiveDescriptors = stage1PrimitiveDescriptors,
      requiredStdCapabilities = Set(
        Core0Stage,
        Core0Text,
        Core0TextOutput,
        Core0OptionResultVec,
        Core0PathFs,
        Core0CharClass,
      ),
      requiredBackendRequirements = Set(
        BackendRequirement.runtimeSymbol("cosmo0_runtime::println"),
        BackendRequirement.runtimeSymbol("cosmo0_runtime::read_file"),
        BackendRequirement.include("<cstdio>"),
        BackendRequirement.include("<fstream>"),
      ),
    )

  val profiles: Map[String, StageCapabilityProfile] =
    List(stage1Profile).map(profile => profile.name -> profile).toMap

  val defaultAvailability: StageCapabilityAvailability =
    StageCapabilityAvailability(
      primitiveDescriptors = StandardGenericDescriptors.Boundary.primitiveRuntimeDescriptorNames + "Unit",
      stdCapabilities = stage1Profile.requiredStdCapabilities,
      backendRequirements = stage1Profile.requiredBackendRequirements,
    )

  def profile(name: String): Option[StageCapabilityProfile] =
    profiles.get(name)

  def validate(
      profileName: String,
      availability: StageCapabilityAvailability = defaultAvailability,
  ): List[Diagnostic] =
    profile(profileName) match
      case Some(value) => validate(value, availability)
      case None =>
        List(
          diagnostic(
            "cosmo0.stage.unknown-profile",
            s"unknown cosmo0 stage capability profile $profileName",
          ),
        )

  def validate(
      profile: StageCapabilityProfile,
      availability: StageCapabilityAvailability,
  ): List[Diagnostic] =
    val missingPrimitiveDescriptors =
      (profile.requiredPrimitiveDescriptors -- availability.primitiveDescriptors).toList.sorted.map { name =>
        diagnostic(
          "cosmo0.stage.missing-capability",
          s"stage profile ${profile.name} requires primitive descriptor $name",
        )
      }

    val missingStdCapabilities =
      (profile.requiredStdCapabilities -- availability.stdCapabilities).toList.sorted.map { name =>
        diagnostic(
          "cosmo0.stage.missing-capability",
          s"stage profile ${profile.name} requires std capability $name",
        )
      }

    val missingBackendRequirements =
      (profile.requiredBackendRequirements -- availability.backendRequirements).toList
        .sortBy(_.legacyName)
        .map { requirement =>
          diagnostic(
            "cosmo0.stage.missing-capability",
            s"stage profile ${profile.name} requires backend ${requirement.legacyName}",
          )
        }

    missingPrimitiveDescriptors ::: missingStdCapabilities ::: missingBackendRequirements

  private def diagnostic(
      code: String,
      message: String,
  ): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      code,
      message,
    )
