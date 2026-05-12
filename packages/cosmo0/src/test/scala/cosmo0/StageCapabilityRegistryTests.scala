package cosmo0

class StageCapabilityRegistryTests extends munit.FunSuite:
  test("Stage 1 profile records primitive, std, and backend requirements"):
    val profile = StageCapabilityRegistry.stage1Profile

    assertEquals(profile.name, StageCapabilityRegistry.Cosmo1Stage1)
    assert(profile.requiredPrimitiveDescriptors.contains("Bool"))
    assert(profile.requiredPrimitiveDescriptors.contains("String"))
    assert(profile.requiredPrimitiveDescriptors.contains("usize"))
    assert(profile.requiredStdCapabilities.contains(StageCapabilityRegistry.Core0Text))
    assert(profile.requiredStdCapabilities.contains(StageCapabilityRegistry.Core0PathFs))
    assert(profile.requiredStdCapabilities.contains(StageCapabilityRegistry.Core0CharClass))
    assert(profile.requiredBackendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::println")))
    assert(profile.requiredBackendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::read_file")))

  test("Stage 1 profile validates against the default capability availability"):
    val diagnostics = StageCapabilityRegistry.validate(StageCapabilityRegistry.Cosmo1Stage1)

    assert(
      diagnostics.isEmpty,
      s"Stage 1 profile validation failed with diagnostics: ${diagnostics.map(d => d.code -> d.message)}",
    )

  test("Stage 1 profile diagnoses missing core0.text"):
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      stdCapabilities = StageCapabilityRegistry.defaultAvailability.stdCapabilities - StageCapabilityRegistry.Core0Text,
    )

    val diagnostics =
      StageCapabilityRegistry.validate(StageCapabilityRegistry.Cosmo1Stage1, availability)

    assertMissingCapability(diagnostics, StageCapabilityRegistry.Core0Text)
    assert(
      diagnostics.exists(_.message.contains("requires std capability core0.text")),
      s"core0.text should be diagnosed as a std capability: ${diagnostics.map(_.message)}",
    )

  test("core0.text is surfaced through std capability metadata, not descriptors"):
    val profile = StageCapabilityRegistry.stage1Profile

    assert(profile.requiredStdCapabilities.contains(StageCapabilityRegistry.Core0Text))
    assert(!profile.requiredPrimitiveDescriptors.contains(StageCapabilityRegistry.Core0Text))
    assert(!profile.requiredPrimitiveDescriptors.contains("TextBuilder"))
    assert(!profile.requiredPrimitiveDescriptors.contains("TextView"))
    assert(!profile.requiredPrimitiveDescriptors.contains("SourceText"))
    assert(StandardGenericDescriptors.get("TextBuilder").isEmpty)
    assert(StandardGenericDescriptors.get("TextView").isEmpty)
    assert(StandardGenericDescriptors.get("SourceText").isEmpty)

  test("Stage 1 profile diagnoses missing core0.path-fs"):
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      stdCapabilities = StageCapabilityRegistry.defaultAvailability.stdCapabilities - StageCapabilityRegistry.Core0PathFs,
    )

    val diagnostics =
      StageCapabilityRegistry.validate(StageCapabilityRegistry.Cosmo1Stage1, availability)

    assertMissingCapability(diagnostics, StageCapabilityRegistry.Core0PathFs)

  test("Stage 1 profile diagnoses missing primitive intrinsics"):
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      primitiveDescriptors = StageCapabilityRegistry.defaultAvailability.primitiveDescriptors - "Bool",
    )

    val diagnostics =
      StageCapabilityRegistry.validate(StageCapabilityRegistry.Cosmo1Stage1, availability)

    assertMissingCapability(diagnostics, "primitive descriptor Bool")

  test("Stage 1 profile diagnoses missing extern backend requirements"):
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      backendRequirements =
        StageCapabilityRegistry.defaultAvailability.backendRequirements -
          BackendRequirement.runtimeSymbol("cosmo0_runtime::read_file"),
    )

    val diagnostics =
      StageCapabilityRegistry.validate(StageCapabilityRegistry.Cosmo1Stage1, availability)

    assertMissingCapability(diagnostics, "runtime-symbol:cosmo0_runtime::read_file")

  test("later-stage capabilities do not block Stage 1 validation"):
    val profile = StageCapabilityRegistry.stage1Profile
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      stdCapabilities =
        StageCapabilityRegistry.defaultAvailability.stdCapabilities --
          StageCapabilityRegistry.laterStageStdCapabilities,
    )

    assertEquals(
      profile.requiredStdCapabilities.intersect(StageCapabilityRegistry.laterStageStdCapabilities),
      Set.empty[String],
    )
    assertEquals(StageCapabilityRegistry.validate(profile, availability), Nil)

  private def assertMissingCapability(
      diagnostics: List[Diagnostic],
      expected: String,
  ): Unit =
    assert(
      diagnostics.exists(diagnostic =>
        diagnostic.code == "cosmo0.stage.missing-capability" &&
          diagnostic.message.contains(expected),
      ),
      s"missing capability diagnostic for $expected in ${diagnostics.map(d => d.code -> d.message)}",
    )
