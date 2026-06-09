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

  test("expression macro payload preserves named arguments"):
    val result = Cosmo0().check("val copied: u8 = example.named(value = 7)")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val copied = result.value.get.typed.decls.collectFirst {
      case value: TypedValueDecl if value.name == "copied" => value
    }.get
    val init = copied.init.get.asInstanceOf[TypedIntLiteral]

    assertEquals(copied.ty, SourceType.Byte)
    assertEquals(init.value, BigInt(7))
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "named=[value=7]",
      ),
    )
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "expr example.named -> value",
      ),
    )

  test("free expression macro does not match a method selector by text"):
    val result = Cosmo0().check(
      """class Example {
        |  val answer: i32
        |}
        |
        |val example = Example(1)
        |val value = example.answer()
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.type.invalid-call"),
      s"missing invalid-call diagnostic in ${result.diagnostics.map(_.code)}",
    )
    assert(
      result.value.isEmpty,
      "failed check should not expose a checked module",
    )

  test("method-like expression macro preserves receiver in Args payload"):
    val result = Cosmo0().check(
      """class MacroBox {}
        |
        |val box = MacroBox()
        |val expanded: u8 = box.expand(9)
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val expanded = result.value.get.typed.decls.collectFirst {
      case value: TypedValueDecl if value.name == "expanded" => value
    }.get
    val init = expanded.init.get.asInstanceOf[TypedIntLiteral]

    assertEquals(expanded.ty, SourceType.Byte)
    assertEquals(init.value, BigInt(9))
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "receiver=box",
      ),
    )
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "expr example.MacroBox.expand -> receiver.arg0",
      ),
    )

  test("block-attached expression macro receives a Block payload"):
    val result = Cosmo0().check(
      """val answer: u8 = example.block {
        |  1
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val answer = result.value.get.typed.decls.collectFirst {
      case value: TypedValueDecl if value.name == "answer" => value
    }.get
    val init = answer.init.get.asInstanceOf[TypedIntLiteral]

    assertEquals(answer.ty, SourceType.Byte)
    assertEquals(init.value, BigInt(42))
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "Expr.Block",
      ),
    )

  test("macro call rejects multiple surface payloads"):
    val result = Cosmo0().check("val answer = example.block(1) { 2 }")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.macro.unsupported-payload",
      ),
      s"missing unsupported-payload diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("template expression macro receives a Template payload"):
    val result = Cosmo0().check("""val message: String = example.text"hello"""")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val message = result.value.get.typed.decls.collectFirst {
      case value: TypedValueDecl if value.name == "message" => value
    }.get
    val init = message.init.get.asInstanceOf[TypedStringLiteral]

    assertEquals(message.ty, SourceType.String)
    assertEquals(init.value, "hello")
    assert(
      result.value.get.macroExpansion.stableDisplay.contains(
        "Expr.Template",
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

  test("expression macro expansion rejects typed-expression injection"):
    val result = Cosmo0().check("val answer = example.typed()")

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.macro.invalid-output"),
      s"missing invalid-output diagnostic in ${result.diagnostics.map(_.code)}",
    )
    assert(
      result.diagnostics.exists(
        _.message.contains("typed expression injection"),
      ),
      s"missing typed-injection diagnostic in ${result.diagnostics.map(_.message)}",
    )

  test("expression macro expansion rejects recursive expansion"):
    val result = Cosmo0().check("val answer = example.recursive()")

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.macro.expansion-cycle",
      ),
      s"missing expansion-cycle diagnostic in ${result.diagnostics.map(_.code)}",
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

  test(
    "expression macro provider registry reports duplicate disabled and invalid entries",
  ):
    object BadSignatureProvider extends CompilerHostedExpressionProvider:
      val id = "example.bad-signature"
      override val signature =
        MacroProviderSignature("TypedExpr", "Expr[Untyped]")
      def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
        MacroFunctionOutput(Nil, None, Nil, Nil, Nil)

    val registry = new MacroExpressionProviderRegistry(
      List(
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.dup"),
          ExampleAnswerExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.dup"),
          ExampleIdentityExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.disabled"),
          ExampleInvalidExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.bad-signature"),
          BadSignatureProvider,
        ),
      ),
      disabledProviderIdentities = Set(ExampleInvalidExpressionProvider.id),
    )

    assertEquals(
      registry.diagnostics.map(_.code),
      List(
        "cosmo0.macro.duplicate-provider",
        "cosmo0.macro.disabled-provider",
        "cosmo0.macro.invalid-provider-signature",
      ),
    )

  test("disabled expression macro provider is rejected at invocation"):
    val elaborated =
      Cosmo0().elaborate("val answer = example.answer()").value.get
    val registry = new MacroExpressionProviderRegistry(
      List(
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.answer"),
          ExampleAnswerExpressionProvider,
        ),
      ),
      disabledProviderIdentities = Set(ExampleAnswerExpressionProvider.id),
    )
    val result =
      new MlttTyper(
        elaborated,
        StandardGenericDescriptors.all,
        CheckerProfiles.MlttDependentPatterns,
        registry,
      ).check()

    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.macro.disabled-provider"),
      s"missing disabled-provider diagnostic in ${result.diagnostics.map(_.code)}",
    )
