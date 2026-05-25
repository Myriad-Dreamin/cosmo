package cosmo0

class PackageRunTests extends munit.FunSuite:
  test("package run compile accepts a package-owned top-level main entrypoint"):
    val result =
      Cosmo0().compileRunnablePackage("fixtures/cosmo0/package/run-smoke")

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"package run compile failed with diagnostics: ${result.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val output = result.value.get.output
    assertEquals(output.moduleName, "cosmo0_package_run_smoke")
    assert(output.source.contains("int main()"))
    assert(
      output.source.contains(
        "return static_cast<int>(cosmo0::cosmo0_package_run_smoke::main());",
      ),
    )

  test("package run reports missing and invalid runnable entrypoints"):
    val missing =
      Cosmo0().checkRunnablePackage("fixtures/cosmo0/package/no-run-entrypoint")

    assertEquals(missing.phase, Phase.Check)
    assertEquals(missing.status, PhaseStatus.Failed)
    assert(
      missing.diagnostics.exists(
        _.code == "cosmo0.package.missing-run-entrypoint",
      ),
      s"missing run entrypoint diagnostic in ${missing.diagnostics.map(_.code)}",
    )

    val invalid = Cosmo0().checkRunnablePackage(
      "fixtures/cosmo0/package/invalid-run-entrypoint",
    )

    assertEquals(invalid.phase, Phase.Check)
    assertEquals(invalid.status, PhaseStatus.Failed)
    assert(
      invalid.diagnostics.exists(
        _.code == "cosmo0.package.invalid-run-entrypoint",
      ),
      s"missing invalid run entrypoint diagnostic in ${invalid.diagnostics.map(_.code)}",
    )
    assert(invalid.diagnostics.exists(_.span.nonEmpty))
