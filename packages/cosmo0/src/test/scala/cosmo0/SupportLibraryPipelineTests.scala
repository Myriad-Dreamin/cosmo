package cosmo0

class SupportLibraryPipelineTests extends munit.FunSuite:
  test("support-library ids define Rust target, C symbol, and artifact conventions"):
    val id = SupportLibraryId.parse("uri-sys").fold(fail(_), identity)
    val artifact = SupportLibraryArtifact(id)

    assertEquals(id.crateDirectory, "crates/uri-sys")
    assertEquals(id.rustLibraryTarget, "cosmo_uri_sys")
    assertEquals(id.cSymbolPrefix, "cosmo_uri_sys_")
    assertEquals(artifact.fileName, "libcosmo_uri_sys.a")
    assertEquals(
      artifact.path,
      "target/cosmo/support-libraries/release/uri-sys/libcosmo_uri_sys.a",
    )

  test("extern supportLibrary metadata uses shared identifier validation"):
    val accepted = Cosmo0().elaborate(
      """@extern("c", name = "cosmo_support_smoke_add", supportLibrary = "support-smoke")
        |def support_smoke_add(lhs: i32, rhs: i32): i32
        |""".stripMargin,
    )

    assertEquals(accepted.status, PhaseStatus.Succeeded)
    val fn = accepted.value.get.declarations.head.asInstanceOf[UntypedFunction]
    assertEquals(fn.externBinding.flatMap(_.supportLibrary), Some("support-smoke"))

    val rejected = Cosmo0().elaborate(
      """@extern("c", name = "bad", supportLibrary = "UriSys")
        |def bad(value: i32): i32
        |""".stripMargin,
    )

    assertEquals(rejected.status, PhaseStatus.Unsupported)
    assert(
      rejected.diagnostics.exists(_.code == "cosmo0.elaborate.invalid-extern"),
      s"missing invalid extern diagnostic in ${rejected.diagnostics.map(_.code)}",
    )

  test("backend turns support-library requirements into a link plan"):
    val lowered = Cosmo0().lower(
      SourceFile(
        "support_smoke.cos",
        """@extern("c", name = "cosmo_support_smoke_add", supportLibrary = "support-smoke")
          |def support_smoke_add(lhs: i32, rhs: i32): i32
          |
          |def smoke(value: i32): i32 = {
          |  support_smoke_add(value, 1)
          |}
          |""".stripMargin,
      ),
    )

    assert(
      lowered.isSuccess,
      s"lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val result = CppBackend().emit(lowered.value.get.lir)

    assert(
      result.isSuccess,
      s"C++ emission failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get
    assert(output.source.contains("cosmo_support_smoke_add(value, static_cast<int32_t>(1))"))
    assert(output.backendRequirements.contains(BackendRequirement.supportLibrary("support-smoke")))
    assertEquals(
      output.supportLibraryLinkArguments,
      List("target/cosmo/support-libraries/release/support-smoke/libcosmo_support_smoke.a"),
    )
    assert(output.runtimeRequirements.contains("support-library:support-smoke"))

  test("support-library link plan reports missing and incompatible artifacts"):
    val id = SupportLibraryPipeline.smokeSupportLibrary
    val plan = SupportLibraryLinkPlan
      .fromBackendRequirements(List(BackendRequirement.supportLibrary(id.value)))
      .fold(diagnostics => fail(diagnostics.map(_.message).mkString("\n")), identity)

    val missing = plan.validateArtifacts(Nil)
    assert(
      missing.exists(_.code == "cosmo0.support-library.missing-artifact"),
      s"missing artifact diagnostic not found in ${missing.map(_.code)}",
    )

    val incompatible = plan.validateArtifacts(
      List(SupportLibraryAvailableArtifact(id, plan.items.head.artifact.path, abiVersion = 0)),
    )
    assert(
      incompatible.exists(_.code == "cosmo0.support-library.incompatible-artifact"),
      s"incompatible artifact diagnostic not found in ${incompatible.map(_.code)}",
    )

    val valid = plan.validateArtifacts(
      List(SupportLibraryAvailableArtifact(id, plan.items.head.artifact.path, SupportLibraryPipeline.abiVersion)),
    )
    assertEquals(valid, Nil)

  test("backend diagnoses invalid hand-built support-library requirements"):
    val binding = LirExternBinding(
      TrustedExternAbi.directCAbiName,
      CppQualifiedSymbol.relative("cosmo_support_smoke_add"),
      List(
        BackendRequirement.runtimeSymbol("cosmo_support_smoke_add"),
        BackendRequirement.supportLibrary("SupportSmoke"),
      ),
    )
    val extern = Lir.function(
      "support_smoke_add",
      List(Lir.param("lhs", SourceType.I32), Lir.param("rhs", SourceType.I32)),
      SourceType.I32,
      locals = Nil,
      blocks = Nil,
      externBinding = Some(binding),
    )
    val caller = Lir.function(
      "call_support_smoke",
      Nil,
      SourceType.I32,
      locals = List(Lir.local("value", SourceType.I32)),
      blocks = List(
        Lir.block(
          "entry",
          operations = List(
            LirDirectCall(
              Some(Lir.localId("value")),
              extern.id,
              List(Lir.int(1), Lir.int(2)),
              Lir.signature(List(SourceType.I32, SourceType.I32), SourceType.I32),
            ),
          ),
          terminator = LirReturn(Some(Lir.ref("value", SourceType.I32))),
        ),
      ),
    )

    val result = CppBackend().emit(LirModule("invalid_support_library", List(extern, caller)))

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.support-library.invalid-id"),
      s"missing invalid support-library diagnostic in ${result.diagnostics.map(_.code)}",
    )
