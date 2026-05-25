package cosmo0

class CheckerProfileTests extends munit.FunSuite:
  test(
    "checker profiles declare supported features, rejected features, and artifact kinds",
  ):
    val basic = CheckerProfiles.CosmocBasicExpr
    val subset = CheckerProfiles.Cosmo0Subset
    val mltt = CheckerProfiles.MlttCore
    val dependent = CheckerProfiles.MlttDependentPatterns

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
    assert(subset.supports(CheckerProfiles.DependentPatternsFeature))
    assert(subset.supports(CheckerProfiles.DependentPatternElaborationFeature))
    assert(!subset.rejects(CheckerProfiles.DependentPatternsFeature))

    assertEquals(mltt.id, "mltt.core")
    assertEquals(mltt.artifactKind, "mltt-core-term")
    assert(mltt.supports("sigma-types"))
    assert(mltt.supports("equality-types"))
    assert(mltt.supports("whnf-conversion"))
    assertEquals(
      mltt.rejectedFeatureCode(CheckerProfiles.ObjectDispatchFeature),
      CheckerProfiles.UnsupportedObjectDispatchCode,
    )
    assertEquals(
      mltt.rejectedFeatureCode(CheckerProfiles.AsyncFeature),
      CheckerProfiles.UnsupportedEffectRowCode,
    )
    assert(mltt.rejects(CheckerProfiles.StagingFeature))
    assert(!mltt.supports(CheckerProfiles.DependentPatternElaborationFeature))
    assertEquals(dependent.id, "mltt.dependent-patterns")
    assertEquals(dependent.artifactKind, "mltt-case-tree")
    assert(dependent.supports(CheckerProfiles.DependentPatternsFeature))
    assert(
      dependent.supports(CheckerProfiles.DependentPatternElaborationFeature),
    )
    assert(!dependent.rejects(CheckerProfiles.DependentPatternsFeature))

  test("default cosmo0 check result uses the cosmo0 subset profile"):
    val result = Cosmo0().check("val answer = 42")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)
    assertEquals(result.value.get.typed.decls.length, 1)

  test("explicit cosmo0 subset profile keeps the default checker behavior"):
    val result = Cosmo0().checkWithProfile(
      "val answer = 42",
      CheckerProfiles.Cosmo0Subset.id,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

  test(
    "experimental MLTT profile rejects object source as an unsupported checker result",
  ):
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
    assertEquals(
      result.diagnostics.head.code,
      CheckerProfiles.UnsupportedObjectDispatchCode,
    )
    assert(result.diagnostics.head.message.contains("mltt.core"))

  test("unknown checker profile is a normal check failure"):
    val result = Cosmo0().checkWithProfile("val answer = 42", "unknown.profile")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.head.code,
      "cosmo.type.unknown-checker-profile",
    )

  test(
    "package checker profile metadata can select the default subset checker",
  ):
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
    assert(result.diagnostics.isEmpty)

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
    assertEquals(
      result.diagnostics.head.code,
      CheckerProfiles.UnsupportedObjectDispatchCode,
    )

  test("fixture metadata can select the experimental MLTT profile"):
    val loaded =
      Cosmo0().loadPackage("fixtures/cosmo0/package/mltt-core-profile")

    assertEquals(loaded.phase, Phase.Check)
    assert(
      loaded.isSuccess,
      s"MLTT profile fixture failed to load: ${loaded.diagnostics}",
    )
    assertEquals(
      loaded.value.get.metadata.checkerProfile,
      Some(CheckerProfiles.MlttCore.id),
    )

    val checked = Cosmo0().checkPackage(loaded.value.get)

    assertEquals(checked.phase, Phase.Check)
    assertEquals(checked.status, PhaseStatus.Unsupported)
    assertEquals(
      checked.diagnostics.head.code,
      CheckerProfiles.UnsupportedObjectDispatchCode,
    )

  test("typed artifact profile summary is deterministic"):
    val first =
      Cosmo0().check("val answer = 42").value.get.typed.checkerArtifactSummary
    val second =
      Cosmo0().check("val answer = 42").value.get.typed.checkerArtifactSummary

    assertEquals(first, second)
    assertEquals(first, "decls=1|val:answer:i32")
end CheckerProfileTests
