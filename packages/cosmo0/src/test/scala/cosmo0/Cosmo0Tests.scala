package cosmo0

class Cosmo0Tests extends munit.FunSuite:
  test("facade can be instantiated independently"):
    val compiler = Cosmo0()
    assertNotEquals(compiler, null)

  test("parse accepts source text and returns a parsed module"):
    val result = Cosmo0().parse("val answer = 42")

    assertEquals(result.phase, Cosmo0Phase.Parse)
    assertEquals(result.status, Cosmo0PhaseStatus.Succeeded)
    assert(result.value.nonEmpty)
    assertEquals(result.value.get.source.name, "<memory>")
    assert(result.diagnostics.isEmpty)

  test("parse failures return structured diagnostics"):
    val result = Cosmo0().parse("val =")

    assertEquals(result.phase, Cosmo0Phase.Parse)
    assertEquals(result.status, Cosmo0PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.parse.failed")
    assert(result.diagnostics.head.span.nonEmpty)

  test("check returns a structured pending result"):
    val result = Cosmo0().check("val answer = 42")

    assertEquals(result.phase, Cosmo0Phase.Check)
    assertEquals(result.status, Cosmo0PhaseStatus.Pending)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.check.pending")

  test("elaborate builds untyped representation for accepted core declarations"):
    val result = Cosmo0().elaborate(
      """type SourceId = Id[SourceFile]
        |
        |class Severity {
        |  case Note
        |  case Error(String)
        |}
        |
        |class Diagnostic {
        |  val labels: Vec[DiagnosticLabel]
        |  var count: usize = 0
        |
        |  def push(&mut self, label: DiagnosticLabel): Unit = {
        |    val previous = self.count;
        |    self.labels.push(label);
        |    if (previous == 0) {
        |      return previous
        |    } else {
        |      self.count = previous + 1
        |    }
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Cosmo0Phase.Check)
    assertEquals(result.status, Cosmo0PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val module = result.value.get
    assertEquals(module.declarations.map(_.name), List("SourceId", "Severity", "Diagnostic"))

    val alias = module.declarations.head.asInstanceOf[Cosmo0UntypedTypeAlias]
    assert(alias.target.isInstanceOf[Cosmo0UntypedAppliedType])

    val severity = module.declarations(1).asInstanceOf[Cosmo0UntypedClass]
    assertEquals(
      severity.members.collect { case variant: Cosmo0UntypedVariant => variant.name },
      List("Note", "Error"),
    )

    val diagnostic = module.declarations(2).asInstanceOf[Cosmo0UntypedClass]
    val push = diagnostic.members.collectFirst { case fn: Cosmo0UntypedFunction => fn }.get
    val selfType = push.params.head.valueType.get.asInstanceOf[Cosmo0UntypedRefType]
    assert(selfType.mutable)
    assert(push.body.exists(_.isInstanceOf[Cosmo0UntypedBlock]))

  test("elaborate represents match arms and standard-generic variant constructors"):
    val result = Cosmo0().elaborate(
      """def current(&self): Option[Token] = {
        |  val current = self.advance();
        |  current match {
        |    case Option[Token]::Some(token) => {
        |      token
        |    }
        |    case Option[Token]::None => {
        |      return token
        |    }
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.status, Cosmo0PhaseStatus.Succeeded)
    val fn = result.value.get.declarations.head.asInstanceOf[Cosmo0UntypedFunction]
    val body = fn.body.get.asInstanceOf[Cosmo0UntypedBlock]
    val matchExpr = body.items.collectFirst { case m: Cosmo0UntypedMatch => m }.get

    assertEquals(matchExpr.arms.length, 2)
    val somePattern = matchExpr.arms.head.pattern.asInstanceOf[Cosmo0UntypedVariantPattern]
    assert(somePattern.constructor.isInstanceOf[Cosmo0UntypedVariantConstructor])
    assertEquals(somePattern.args.length, 1)

  test("elaborate rejects unsupported full-language constructs deterministically"):
    val cases = List(
      "trait Display {}" -> "cosmo0.elaborate.unsupported.trait",
      "class Arena[T] {}" -> "cosmo0.elaborate.unsupported.generic-class",
      "def id[T](x: T): T = x" -> "cosmo0.elaborate.unsupported.generic-function",
      "impl Display {}" -> "cosmo0.elaborate.unsupported.impl",
      "def f(T: Type): Unit = {}" -> "cosmo0.elaborate.unsupported.host-type",
      "type X = User[T]" -> "cosmo0.elaborate.unsupported.generic-type",
      "def f(): Unit = { val x = Vec[i32] }" ->
        "cosmo0.elaborate.unsupported.compile-time-apply",
      "def f(): Unit = { val f = x => x }" ->
        "cosmo0.elaborate.unsupported.lambda",
      "def f(items: Vec[i32]): Unit = { items.map(process) }" ->
        "cosmo0.elaborate.unsupported.higher-order-api",
    )

    cases.foreach { case (source, code) =>
      val result = Cosmo0().elaborate(source)

      assertEquals(result.phase, Cosmo0Phase.Check)
      assertEquals(result.status, Cosmo0PhaseStatus.Unsupported)
      assert(result.value.isEmpty)
      assert(
        result.diagnostics.exists(_.code == code),
        s"missing diagnostic $code in ${result.diagnostics.map(_.code)}",
      )
    }

  test("check stops on unsupported subset constructs"):
    val result = Cosmo0().check("trait Display {}")

    assertEquals(result.phase, Cosmo0Phase.Check)
    assertEquals(result.status, Cosmo0PhaseStatus.Unsupported)
    assertEquals(result.diagnostics.head.code, "cosmo0.elaborate.unsupported.trait")

  test("elaboration preserves spans for accepted nodes and diagnostics"):
    val accepted = Cosmo0().elaborate("\nval answer = 42")
    val declarationSpan = accepted.value.get.declarations.head.span

    assertEquals(declarationSpan.start.line, 2)
    assertEquals(declarationSpan.start.column, 1)
    assert(declarationSpan.end.offset > declarationSpan.start.offset)

    val rejected = Cosmo0().elaborate(
      """def f(): Unit = {
        |  val f = x => x
        |}
        |""".stripMargin,
    )
    val diagnosticSpan = rejected.diagnostics.head.span.get

    assertEquals(diagnosticSpan.start.line, 2)
    assert(diagnosticSpan.end.offset >= diagnosticSpan.start.offset)

  test("compile returns a structured unsupported result"):
    val result = Cosmo0().compile("val answer = 42")

    assertEquals(result.phase, Cosmo0Phase.Compile)
    assertEquals(result.status, Cosmo0PhaseStatus.Unsupported)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.compile.unsupported")

  test("compile stops on unsupported subset constructs"):
    val result = Cosmo0().compile("trait Display {}")

    assertEquals(result.phase, Cosmo0Phase.Compile)
    assertEquals(result.status, Cosmo0PhaseStatus.Unsupported)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.elaborate.unsupported.trait")
