package cosmo0

class SupportLibraryPipelineTests extends munit.FunSuite:
  test(
    "support-library ids define Rust target, C symbol, and artifact conventions",
  ):
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

  test("ureq-sys support-library id maps through the shared Rust pipeline"):
    val id = SupportLibraryId.parse("ureq-sys").fold(fail(_), identity)
    val artifact = SupportLibraryArtifact(id)
    val plan = SupportLibraryLinkPlan
      .fromBackendRequirements(
        List(BackendRequirement.supportLibrary(id.value)),
      )
      .fold(
        diagnostics => fail(diagnostics.map(_.message).mkString("\n")),
        identity,
      )

    assertEquals(id.crateDirectory, "crates/ureq-sys")
    assertEquals(id.rustLibraryTarget, "cosmo_ureq_sys")
    assertEquals(id.cSymbolPrefix, "cosmo_ureq_sys_")
    assertEquals(artifact.fileName, "libcosmo_ureq_sys.a")
    assertEquals(
      artifact.path,
      "target/cosmo/support-libraries/release/ureq-sys/libcosmo_ureq_sys.a",
    )
    assertEquals(
      plan.linkArguments,
      List(
        "target/cosmo/support-libraries/release/ureq-sys/libcosmo_ureq_sys.a",
      ),
    )

  test("uri-sys support-library id maps through the shared Rust pipeline"):
    val id = SupportLibraryId.parse("uri-sys").fold(fail(_), identity)
    val artifact = SupportLibraryArtifact(id)
    val plan = SupportLibraryLinkPlan
      .fromBackendRequirements(
        List(BackendRequirement.supportLibrary(id.value)),
      )
      .fold(
        diagnostics => fail(diagnostics.map(_.message).mkString("\n")),
        identity,
      )

    assertEquals(id.crateDirectory, "crates/uri-sys")
    assertEquals(id.rustLibraryTarget, "cosmo_uri_sys")
    assertEquals(id.cSymbolPrefix, "cosmo_uri_sys_")
    assertEquals(artifact.fileName, "libcosmo_uri_sys.a")
    assertEquals(
      artifact.path,
      "target/cosmo/support-libraries/release/uri-sys/libcosmo_uri_sys.a",
    )
    assertEquals(
      plan.linkArguments,
      List("target/cosmo/support-libraries/release/uri-sys/libcosmo_uri_sys.a"),
    )

  test(
    "ureq-sys package declarations check as a cosmo0 support-library surface",
  ):
    val checked = Cosmo0().checkPackage("packages/ureq-sys")

    assert(
      checked.isSuccess,
      s"ureq-sys package check failed with diagnostics: ${checked.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assertEquals(checked.value.get.metadata.name, "@cosmo/ureq-sys")
    assertEquals(checked.value.get.moduleOrder.lastOption, Some("ureq_sys"))

    val functions = checked.value.get.checked.typed.decls.collect {
      case fn: TypedFunction =>
        fn.name -> fn.extern.flatMap(_.supportLibrary)
    }
    val classes = checked.value.get.checked.typed.decls.collect {
      case cls: TypedClass =>
        cls.name -> cls.methods.map(_.name)
    }.toMap
    assert(functions.contains("ureq_request" -> None))
    assert(functions.contains("ureq_byte_slice" -> None))
    assert(
      functions.contains("unsafe_ureq_sys_request_new" -> Some("ureq-sys")),
    )
    assert(
      functions.contains(
        "unsafe_ureq_sys_response_body_bytes" -> Some("ureq-sys"),
      ),
    )
    assert(
      functions.contains("unsafe_ureq_sys_error_release" -> Some("ureq-sys")),
    )
    List("UreqOwnedBytes", "UreqError", "UreqRequest", "UreqResponse").foreach {
      name =>
        assert(
          classes.get(name).exists(_.contains("drop")),
          s"$name is missing Drop.drop in ${classes.get(name)}",
        )
        assert(
          !classes.get(name).exists(_.contains("release")),
          s"$name still exposes release in ${classes.get(name)}",
        )
    }

    val source =
      ParserFixtureManifest.readFile("packages/ureq-sys/src/ureq_sys.cos")
    val elaborated = Cosmo0().elaborate(SourceFile("ureq_sys.cos", source))
    assert(
      elaborated.isSuccess,
      s"ureq-sys elaboration failed with diagnostics: ${elaborated.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val declarations = elaborated.value.get.decls
    val publicFunctions = declarations.collect {
      case fn: UntypedFunction if fn.vis == UntypedVisibility.Public => fn.name
    }
    val unsafeExterns = declarations.collect {
      case fn: UntypedFunction if fn.name.startsWith("unsafe_ureq_sys_") =>
        fn.name -> (fn.vis, fn.extern.flatMap(_.supportLibrary))
    }
    val publicClasses = declarations.collect {
      case cls: UntypedClass if cls.vis == UntypedVisibility.Public => cls.name
    }
    val publicTraits = declarations.collect {
      case trt: UntypedTrait if trt.vis == UntypedVisibility.Public => trt.name
    }
    val privateClasses = declarations.collect {
      case cls: UntypedClass if cls.vis == UntypedVisibility.Private => cls.name
    }

    assert(publicFunctions.contains("ureq_request"))
    assert(publicFunctions.contains("ureq_byte_slice"))
    assert(!publicFunctions.exists(_.startsWith("unsafe_ureq_sys_")))
    assert(unsafeExterns.nonEmpty)
    assert(
      unsafeExterns.forall { case (_, (visibility, supportLibrary)) =>
        visibility == UntypedVisibility.Private && supportLibrary.contains(
          "ureq-sys",
        )
      },
      s"unsafe extern visibility/linkage mismatch: $unsafeExterns",
    )

    assert(publicClasses.contains("UreqByteSlice"))
    assert(publicClasses.contains("UreqOwnedBytes"))
    assert(publicClasses.contains("UreqError"))
    assert(publicClasses.contains("UreqRequest"))
    assert(publicClasses.contains("UreqResponse"))
    assert(!publicClasses.exists(_.startsWith("UreqSys")))
    assert(publicTraits.contains("Drop"))
    assert(privateClasses.contains("UreqSysRawRequest"))
    assert(privateClasses.contains("UreqSysRawResponse"))
    assert(privateClasses.contains("UreqSysRawError"))
    assert(privateClasses.contains("UreqSysBytes"))

    val privateRawWrappers =
      Set("UreqOwnedBytes", "UreqError", "UreqRequest", "UreqResponse")
    val rawFieldVisibility = declarations.collect {
      case cls: UntypedClass if privateRawWrappers.contains(cls.name) =>
        cls.name -> cls.members.collectFirst {
          case field: UntypedValueDecl if field.name == "raw" =>
            field.vis
        }
    }
    assertEquals(
      rawFieldVisibility.toMap,
      privateRawWrappers.map(_ -> Some(UntypedVisibility.Private)).toMap,
    )

  test(
    "uri-sys package declarations check as a cosmo0 support-library surface",
  ):
    val checked = Cosmo0().checkPackage("packages/uri-sys")

    assert(
      checked.isSuccess,
      s"uri-sys package check failed with diagnostics: ${checked.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assertEquals(checked.value.get.metadata.name, "@cosmo/uri-sys")
    assertEquals(checked.value.get.moduleOrder.lastOption, Some("uri_sys"))

    val functions = checked.value.get.checked.typed.decls.collect {
      case fn: TypedFunction =>
        fn.name -> fn.extern.flatMap(_.supportLibrary)
    }
    val classes = checked.value.get.checked.typed.decls.collect {
      case cls: TypedClass =>
        cls.name -> cls.methods.map(_.name)
    }.toMap
    assert(functions.contains("unsafe_uri_sys_parse" -> Some("uri-sys")))
    assert(functions.contains("unsafe_uri_sys_join" -> Some("uri-sys")))
    assert(functions.contains("unsafe_uri_sys_to_file_path" -> Some("uri-sys")))
    assert(
      functions.contains("unsafe_uri_sys_bytes_release" -> Some("uri-sys")),
    )
    assert(functions.contains("uri_sys_parse_string" -> None))
    assert(functions.contains("uri_sys_from_file_path_string" -> None))
    assert(classes.get("Uri").exists(_.contains("parse")))
    assert(classes.get("Uri").exists(_.contains("parse_string")))
    assert(classes.get("Uri").exists(_.contains("from_file_path")))
    assert(classes.get("Uri").exists(_.contains("from_file_path_string")))
    assert(classes.get("Uri").exists(_.contains("display")))
    List("UriOwnedBytes", "UriError", "Uri").foreach { name =>
      assert(
        classes.get(name).exists(_.contains("drop")),
        s"$name is missing Drop.drop in ${classes.get(name)}",
      )
      assert(
        !classes.get(name).exists(_.contains("release")),
        s"$name still exposes release in ${classes.get(name)}",
      )
    }

    val source =
      ParserFixtureManifest.readFile("packages/uri-sys/src/uri_sys.cos")
    val elaborated = Cosmo0().elaborate(SourceFile("uri_sys.cos", source))
    assert(
      elaborated.isSuccess,
      s"uri-sys elaboration failed with diagnostics: ${elaborated.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val declarations = elaborated.value.get.decls
    val publicFunctions = declarations.collect {
      case fn: UntypedFunction if fn.vis == UntypedVisibility.Public => fn.name
    }
    val unsafeExterns = declarations.collect {
      case fn: UntypedFunction if fn.name.startsWith("unsafe_uri_sys_") =>
        fn.name -> (fn.vis, fn.extern.flatMap(_.supportLibrary))
    }
    val publicClasses = declarations.collect {
      case cls: UntypedClass if cls.vis == UntypedVisibility.Public => cls.name
    }
    val publicTraits = declarations.collect {
      case trt: UntypedTrait if trt.vis == UntypedVisibility.Public => trt.name
    }
    val privateClasses = declarations.collect {
      case cls: UntypedClass if cls.vis == UntypedVisibility.Private => cls.name
    }

    assert(!publicFunctions.contains("uri_parse"))
    assert(!publicFunctions.contains("uri_parse_string"))
    assert(!publicFunctions.contains("uri_from_file_path"))
    assert(!publicFunctions.contains("uri_from_file_path_string"))
    assert(publicFunctions.contains("uri_sys_parse_string"))
    assert(publicFunctions.contains("uri_sys_from_file_path_string"))
    assert(!publicFunctions.exists(_.startsWith("unsafe_uri_sys_")))
    assert(unsafeExterns.nonEmpty)
    assert(
      unsafeExterns.forall { case (_, (visibility, supportLibrary)) =>
        visibility == UntypedVisibility.Private && supportLibrary.contains(
          "uri-sys",
        )
      },
      s"unsafe extern visibility/linkage mismatch: $unsafeExterns",
    )

    assert(publicClasses.contains("UriByteSlice"))
    assert(publicClasses.contains("UriOwnedBytes"))
    assert(publicClasses.contains("UriError"))
    assert(publicClasses.contains("Uri"))
    assert(!publicClasses.exists(_.startsWith("UriSys")))
    assert(publicTraits.contains("Drop"))
    assert(privateClasses.contains("UriSysRawUri"))
    assert(privateClasses.contains("UriSysRawError"))
    assert(privateClasses.contains("UriSysBytes"))

    val privateRawWrappers = Set("UriOwnedBytes", "UriError", "Uri")
    val rawFieldVisibility = declarations.collect {
      case cls: UntypedClass if privateRawWrappers.contains(cls.name) =>
        cls.name -> cls.members.collectFirst {
          case field: UntypedValueDecl if field.name == "raw" =>
            field.vis
        }
    }
    assertEquals(
      rawFieldVisibility.toMap,
      privateRawWrappers.map(_ -> Some(UntypedVisibility.Private)).toMap,
    )

  test("extern supportLibrary metadata uses shared identifier validation"):
    val accepted = Cosmo0().elaborate(
      """@extern("c", name = "cosmo_support_smoke_add", supportLibrary = "support-smoke")
        |def support_smoke_add(lhs: i32, rhs: i32): i32
        |""".stripMargin,
    )

    assertEquals(accepted.status, PhaseStatus.Succeeded)
    val fn = accepted.value.get.decls.head.asInstanceOf[UntypedFunction]
    assertEquals(fn.extern.flatMap(_.supportLibrary), Some("support-smoke"))

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

    val result = CppBackend(lowered.value.get.lir).emit()

    assert(
      result.isSuccess,
      s"C++ emission failed with diagnostics: ${result.diagnostics
          .map(d => d.code -> d.message)}",
    )
    val output = result.value.get
    assert(
      output.source.contains(
        "cosmo_support_smoke_add(value, static_cast<int32_t>(1))",
      ),
    )
    assert(
      output.backendRequirements.contains(
        BackendRequirement.supportLibrary("support-smoke"),
      ),
    )
    assertEquals(
      output.supportLibraryLinkArguments,
      List(
        "target/cosmo/support-libraries/release/support-smoke/libcosmo_support_smoke.a",
      ),
    )
    assert(output.runtimeRequirements.contains("support-library:support-smoke"))

  test("support-library link plan reports missing and incompatible artifacts"):
    val id = SupportLibraryPipeline.smokeSupportLibrary
    val plan = SupportLibraryLinkPlan
      .fromBackendRequirements(
        List(BackendRequirement.supportLibrary(id.value)),
      )
      .fold(
        diagnostics => fail(diagnostics.map(_.message).mkString("\n")),
        identity,
      )

    val missing = plan.validateArtifacts(Nil)
    assert(
      missing.exists(_.code == "cosmo0.support-library.missing-artifact"),
      s"missing artifact diagnostic not found in ${missing.map(_.code)}",
    )

    val incompatible = plan.validateArtifacts(
      List(
        SupportLibraryAvailableArtifact(
          id,
          plan.items.head.artifact.path,
          abiVersion = 0,
        ),
      ),
    )
    assert(
      incompatible.exists(
        _.code == "cosmo0.support-library.incompatible-artifact",
      ),
      s"incompatible artifact diagnostic not found in ${incompatible.map(_.code)}",
    )

    val valid = plan.validateArtifacts(
      List(
        SupportLibraryAvailableArtifact(
          id,
          plan.items.head.artifact.path,
          SupportLibraryPipeline.abiVersion,
        ),
      ),
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
              Lir
                .signature(List(SourceType.I32, SourceType.I32), SourceType.I32),
            ),
          ),
          terminator = LirReturn(Some(Lir.ref("value", SourceType.I32))),
        ),
      ),
    )

    val result = CppBackend(
      LirModule("invalid_support_library", List(extern, caller)),
    ).emit()

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.support-library.invalid-id"),
      s"missing invalid support-library diagnostic in ${result.diagnostics.map(_.code)}",
    )
