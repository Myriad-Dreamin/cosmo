package cosmo0

class MacroExpansionTests extends munit.FunSuite:
  private val deriveSource =
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
      |
      |def count(config: Config): i32 = {
      |  config.field_count()
      |}
      |""".stripMargin

  test("elaborate preserves structured derive and field attributes"):
    val result = Cosmo0().elaborate(deriveSource)

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

  test("macro expansion generates deterministic trait impl attachment"):
    val first = Cosmo0().expandMacros(deriveSource)
    val second = Cosmo0().expandMacros(deriveSource)

    assertEquals(first.status, PhaseStatus.Succeeded)
    assertEquals(second.status, PhaseStatus.Succeeded)
    assertEquals(
      first.value.get.summary.stableDisplay,
      second.value.get.summary.stableDisplay,
    )
    assert(
      first.value.get.summary.stableDisplay.contains(
        "derive example.FieldCount -> impl FieldCount for Config",
      ),
    )

    val generatedImpls = first.value.get.module.decls.collect {
      case impl: UntypedImpl => impl
    }
    assertEquals(generatedImpls.length, 1)
    assertEquals(
      MacroStableDisplay.path(generatedImpls.head.traitName),
      "FieldCount",
    )
    assertEquals(MacroStableDisplay.path(generatedImpls.head.target), "Config")

  test("check can use a derive-generated trait method"):
    val result = Cosmo0().check(deriveSource)

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)
    assert(!result.value.get.macroExpansion.isEmpty)

    val count = result.value.get.typed.decls.collectFirst {
      case fn: TypedFunction if fn.name == "count" => fn
    }.get
    assertEquals(count.retTy, SourceType.I32)

  test("compile keeps derive-generated impls on the ordinary backend path"):
    val result = Cosmo0().compile(deriveSource)

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)
    assert(result.value.get.output.contains("field_count"))

  test("macro expansion rejects unresolved derive providers"):
    val result = Cosmo0().check(
      """@derive(missing.Provider)
        |class Config {
        |  val name: String
        |}
        |""".stripMargin,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.macro.unresolved-provider"),
      s"missing unresolved-provider diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("macro expansion rejects derive on unsupported targets"):
    val result = Cosmo0().check(
      """@derive(example.FieldCount)
        |def not_a_class(): i32 = 0
        |""".stripMargin,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.macro.unsupported-target"),
      s"missing unsupported-target diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("macro expansion rejects unconsumed field attributes"):
    val result = Cosmo0().check(
      """trait FieldCount {
        |  def field_count(&self): i32
        |}
        |
        |@derive(example.FieldCount)
        |class Config {
        |  @agr(long = "package")
        |  val package_name: String
        |}
        |""".stripMargin,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.macro.unconsumed-attribute",
      ),
      s"missing unconsumed-attribute diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("macro expansion validates provider-specific attribute payloads"):
    val result = Cosmo0().check(
      """trait FieldCount {
        |  def field_count(&self): i32
        |}
        |
        |@derive(example.FieldCount)
        |class Config {
        |  @arg(123)
        |  val package_name: String
        |}
        |""".stripMargin,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.macro.unsupported-attribute-payload",
      ),
      s"missing unsupported-attribute-payload diagnostic in ${result.diagnostics
          .map(_.code)}",
    )

  test("elaborate rejects repeated keyed macro attribute arguments"):
    val result = Cosmo0().elaborate(
      """class Config {
        |  @arg(long = "package", long = "pkg")
        |  val package_name: String
        |}
        |""".stripMargin,
    )

    assertEquals(result.status, PhaseStatus.Unsupported)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.elaborate.invalid-macro-attribute",
      ),
      s"missing invalid-macro-attribute diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("macro expansion rejects duplicate generated impl attachments"):
    val result = Cosmo0().check(
      """trait FieldCount {
        |  def field_count(&self): i32
        |}
        |
        |@derive(example.FieldCount)
        |class Config {
        |  val package_name: String
        |}
        |
        |impl FieldCount for Config {
        |  def field_count(&self): i32 = 9
        |}
        |""".stripMargin,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.macro.duplicate-impl"),
      s"missing duplicate-impl diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("macro expansion is gated by checker profile support"):
    val result = Cosmo0().expandMacrosWithProfile(
      deriveSource,
      CheckerProfiles.MlttCore.id,
    )

    assertEquals(result.status, PhaseStatus.Unsupported)
    assert(
      result.diagnostics.exists(
        _.code == CheckerProfiles.UnsupportedFeatureCode,
      ),
      s"missing unsupported-feature diagnostic in ${result.diagnostics.map(_.code)}",
    )
