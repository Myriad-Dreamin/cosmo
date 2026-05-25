package cosmo0

class PackageVisibilityTests extends munit.FunSuite:
  test(
    "package check enforces private declarations at named import boundaries",
  ):
    val invalid =
      Cosmo0().checkPackage("fixtures/cosmo0/package/private-import")

    assertEquals(invalid.phase, Phase.Check)
    assertEquals(invalid.status, PhaseStatus.Failed)
    assert(
      invalid.diagnostics.exists(
        _.code == "cosmo0.package.missing-import-member",
      ),
      s"missing private import diagnostic in ${invalid.diagnostics.map(_.code)}",
    )

    val valid = Cosmo0().checkPackage("fixtures/cosmo0/package/private-helper")

    assertEquals(valid.phase, Phase.Check)
    assert(
      valid.isSuccess,
      s"private helper package check failed with diagnostics: ${valid.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assertEquals(valid.value.get.moduleOrder, List("hidden", "main"))
