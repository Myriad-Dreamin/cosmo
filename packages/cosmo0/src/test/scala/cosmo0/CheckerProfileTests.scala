package cosmo0

class CheckerProfileTests extends munit.FunSuite:
  test("checker profiles declare supported features, rejected features, and artifact kinds"):
    val basic = CheckerProfiles.CosmocBasicExpr
    val subset = CheckerProfiles.Cosmo0Subset
    val mltt = CheckerProfiles.MlttCore

    assertEquals(basic.id, "cosmoc.basic-expr")
    assertEquals(basic.artifactKind, "typed-expression-map")
    assert(basic.supports("primitive-expressions"))
    assertEquals(
      basic.rejectedFeatureCode(CheckerProfiles.DependentPatternsFeature),
      CheckerProfiles.UnsupportedDependentPatternCode,
    )

    assertEquals(subset.id, "cosmo0.subset")
    assertEquals(subset.artifactKind, "typed-module")
    assert(subset.supports("cosmo0-expressions"))

    assertEquals(mltt.id, "mltt.core")
    assertEquals(mltt.artifactKind, "mltt-core-term")
    assertEquals(
      mltt.rejectedFeatureCode(CheckerProfiles.ObjectDispatchFeature),
      CheckerProfiles.UnsupportedObjectDispatchCode,
    )

  test("default cosmo0 check result identifies the cosmo0 subset profile"):
    val result = Cosmo0().check("val answer = 42")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assertEquals(result.value.get.typed.checkerProfileId, CheckerProfiles.Cosmo0Subset.id)
    assertEquals(result.value.get.typed.checkerArtifactKind, "typed-module")

  test("explicit cosmo0 subset profile keeps the default checker behavior"):
    val result = Cosmo0().checkWithProfile("val answer = 42", CheckerProfiles.Cosmo0Subset.id)

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assertEquals(result.value.get.typed.checkerProfileId, CheckerProfiles.Cosmo0Subset.id)
    assert(result.diagnostics.isEmpty)

  test("experimental MLTT profile rejects object source as an unsupported checker result"):
    val result = Cosmo0().checkWithProfile(
      """class Box {
        |  val value: i32
        |}
        |""".stripMargin,
      CheckerProfiles.MlttCore.id,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assertEquals(result.value, None)
    assertEquals(result.diagnostics.head.code, CheckerProfiles.UnsupportedObjectDispatchCode)
    assert(result.diagnostics.head.message.contains("mltt.core"))

  test("unknown checker profile is a normal check failure"):
    val result = Cosmo0().checkWithProfile("val answer = 42", "unknown.profile")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(result.diagnostics.head.code, "cosmo.type.unknown-checker-profile")

  test("package checker profile metadata can select the default subset checker"):
    val source = SourceFile("main.cos", "val answer = 42")
    val pkg = Cosmo0Package(
      "memory",
      Cosmo0PackageMetadata(
        "@cosmo0/profile-smoke",
        "0.0.0",
        checkerProfile = Some(CheckerProfiles.Cosmo0Subset.id),
      ),
      List(Cosmo0PackageModule(List("main"), source)),
    )

    val result = Cosmo0().checkPackage(pkg)

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assertEquals(result.value.get.checked.typed.checkerProfileId, CheckerProfiles.Cosmo0Subset.id)

  test("package checker profile metadata keeps experimental profiles isolated"):
    val source = SourceFile("main.cos", "val answer = 42")
    val pkg = Cosmo0Package(
      "memory",
      Cosmo0PackageMetadata(
        "@cosmo0/profile-smoke",
        "0.0.0",
        checkerProfile = Some(CheckerProfiles.MlttCore.id),
      ),
      List(Cosmo0PackageModule(List("main"), source)),
    )

    val result = Cosmo0().checkPackage(pkg)

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assertEquals(result.diagnostics.head.code, CheckerProfiles.UnsupportedObjectDispatchCode)

  test("typed artifact profile summary is deterministic"):
    val first = Cosmo0().check("val answer = 42").value.get.typed.checkerArtifactSummary
    val second = Cosmo0().check("val answer = 42").value.get.typed.checkerArtifactSummary

    assertEquals(first, second)
    assertEquals(first, "cosmo0.subset|typed-module|decls=1|val:answer:i32")
end CheckerProfileTests
