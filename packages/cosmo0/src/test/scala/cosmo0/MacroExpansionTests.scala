package cosmo0

class MacroExpansionTests extends munit.FunSuite:
  private val attributeSource =
    """trait FieldCount {
      |  def field_count(&self): i32
      |}
      |
      |@derive(example.FieldCount)
      |class Config {
      |  @arg(long = "package", short = "p")
      |  val package_name: String
      |
      |  val verbose: Bool
      |}
      |""".stripMargin

  test("elaborate preserves structured derive and field attributes"):
    val result = Cosmo0().elaborate(attributeSource)

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val cls = result.value.get.decls.collectFirst {
      case cls: UntypedClass if cls.name == "Config" => cls
    }.get
    assertEquals(
      cls.macroAttributes.map(_.stableDisplay),
      List("@derive(example.FieldCount)"),
    )

    val packageField = cls.members.collectFirst {
      case field: UntypedValueDecl if field.name == "package_name" => field
    }.get
    assertEquals(
      packageField.macroAttributes.map(_.stableDisplay),
      List("""@arg(long="package",short="p")"""),
    )

  test("check expands expression macro during expression typing"):
    val result = Cosmo0().check("val answer: u8 = example.answer()")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val answer = result.value.get.typed.decls.collectFirst {
      case value: TypedValueDecl if value.name == "answer" => value
    }.get
    val init = answer.init.get.asInstanceOf[TypedIntLiteral]

    assertEquals(answer.ty, SourceType.Byte)
    assertEquals(init.value, BigInt(42))
    assertEquals(init.ty, SourceType.Byte)
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "expr example.answer -> 42",
      ),
    )

  test("macro expansion summary is deterministic"):
    val source = "val answer: u8 = example.answer()"
    val first = Cosmo0().check(source)
    val second = Cosmo0().check(source)

    assertEquals(first.status, PhaseStatus.Succeeded)
    assertEquals(second.status, PhaseStatus.Succeeded)
    assertEquals(
      first.value.get.macroExpansion.stableDisplay,
      second.value.get.macroExpansion.stableDisplay,
    )
    assert(
      first.value.get.macroExpansion.stableDisplay.contains(
        "expr example.answer -> 42",
      ),
    )

  test("identity expression macro output is rechecked in caller context"):
    val result = Cosmo0().check("val copied: u8 = example.identity(41)")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val copied = result.value.get.typed.decls.collectFirst {
      case value: TypedValueDecl if value.name == "copied" => value
    }.get
    val init = copied.init.get.asInstanceOf[TypedIntLiteral]

    assertEquals(copied.ty, SourceType.Byte)
    assertEquals(init.value, BigInt(41))
    assertEquals(init.ty, SourceType.Byte)
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "expr example.identity -> arg0",
      ),
    )

  test("compile keeps expression macro output on the ordinary backend path"):
    val result = Cosmo0().compile("val answer = example.answer()")

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)
    assert(
      result.value.get.output
        .contains("inline const int32_t answer = static_cast<int32_t>(42);"),
    )

  test("expression macro expansion rejects unresolved providers"):
    val result = Cosmo0().check("val answer = example.missing()")

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.macro.unresolved-provider"),
      s"missing unresolved-provider diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("expression macro expansion rejects invalid provider output"):
    val result = Cosmo0().check("val answer = example.invalid()")

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.macro.invalid-output"),
      s"missing invalid-output diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("expression macro expansion rejects unsupported provider inputs"):
    val result = Cosmo0().check("val answer = example.answer(1)")

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.macro.unsupported-input",
      ),
      s"missing unsupported-input diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("expression macro expansion is gated by checker profile support"):
    val elaborated =
      Cosmo0().elaborate("val answer = example.answer()").value.get
    val result = MlttTyper(elaborated, CheckerProfiles.MlttCore).check()

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(
        _.code == CheckerProfiles.UnsupportedFeatureCode,
      ),
      s"missing unsupported-feature diagnostic in ${result.diagnostics.map(_.code)}",
    )
