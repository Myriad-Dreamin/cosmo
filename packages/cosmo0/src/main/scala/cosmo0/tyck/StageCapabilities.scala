package cosmo0

/** Runtime and standard-library capabilities required by a package stage.
  *
  * `stageProfile` is separate from `checkerProfile`: stage validation answers
  * "does the current package target have enough runtime/std support?", while
  * checker profiles answer "which type-checker artifact contract is selected?".
  *
  * Package metadata example:
  *
  * {{{
  * {
  *   "name": "@cosmo/compiler",
  *   "target": "cosmo0",
  *   "stageProfile": "cosmo1.stage1"
  * }
  * }}}
  *
  * Validation rule:
  *
  *   - every required primitive descriptor must be available,
  *   - every required std capability must be available, and
  *   - every required backend symbol/include must be available.
  */
final case class StageCapabilityProfile(
    name: String,
    requiredPrimitiveDescriptors: Set[String],
    requiredStdCapabilities: Set[String],
    requiredBackendRequirements: Set[BackendRequirement] = Set.empty,
)

/** Concrete capabilities supplied by the active compiler/runtime boundary. */
final case class StageCapabilityAvailability(
    primitiveDescriptors: Set[String],
    stdCapabilities: Set[String],
    backendRequirements: Set[BackendRequirement],
)

/** Registry for package stage profiles.
  *
  * The registry does no expression or type inference. It produces check-phase
  * diagnostics before source type checking when a package asks for capabilities
  * that the current cosmo0 target cannot provide.
  */
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

  val knownStdCapabilities: Set[String] =
    Set(
      Core0Stage,
      Core0Text,
      Core0TextOutput,
      Core0OptionResultVec,
      Core0PathFs,
      Core0CharClass,
    ) ++ laterStageStdCapabilities

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
        BackendRequirement.runtimeSymbol("cosmo0_runtime::print"),
        BackendRequirement.runtimeSymbol("cosmo0_runtime::println"),
        BackendRequirement.runtimeSymbol("cosmo0_runtime::read_file"),
        BackendRequirement.runtimeSymbol("cosmo0_runtime::read_dir"),
        BackendRequirement.runtimeSymbol("cosmo0_runtime::path_is_file"),
        BackendRequirement.runtimeSymbol("cosmo0_runtime::path_is_dir"),
        BackendRequirement.include("<cstdio>"),
        BackendRequirement.include("<fstream>"),
        BackendRequirement.include("<filesystem>"),
      ),
    )

  val profiles: Map[String, StageCapabilityProfile] =
    List(stage1Profile).map(profile => profile.name -> profile).toMap

  val defaultAvailability: StageCapabilityAvailability =
    StageCapabilityAvailability(
      primitiveDescriptors =
        StandardGenericDescriptors.Boundary.primitiveRuntimeDescriptorNames + "Unit",
      stdCapabilities = knownStdCapabilities,
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
      (profile.requiredPrimitiveDescriptors -- availability.primitiveDescriptors).toList.sorted
        .map { name =>
          diagnostic(
            "cosmo0.stage.missing-capability",
            s"stage profile ${profile.name} requires primitive descriptor $name",
          )
        }

    val missingStdCapabilities =
      (profile.requiredStdCapabilities -- availability.stdCapabilities).toList.sorted
        .map { name =>
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
