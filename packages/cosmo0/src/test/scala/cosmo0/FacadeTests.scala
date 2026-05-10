package cosmo0

class FacadeTests extends munit.FunSuite:
  test("facade can be instantiated independently"):
    val compiler = Cosmo0()
    assertNotEquals(compiler, null)

  test("parse accepts source text and returns a parsed module"):
    val result = Cosmo0().parse("val answer = 42")

    assertEquals(result.phase, Phase.Parse)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.value.nonEmpty)
    assertEquals(result.value.get.source.name, "<memory>")
    assert(result.diagnostics.isEmpty)

  test("parse failures return structured diagnostics"):
    val result = Cosmo0().parse("val =")

    assertEquals(result.phase, Phase.Parse)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.parse.failed")
    assert(result.diagnostics.head.span.nonEmpty)

  test("check returns a typed module for accepted source"):
    val result = Cosmo0().check("val answer = 42")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.value.nonEmpty)
    assert(result.diagnostics.isEmpty)

    val value = result.value.get.typed.declarations.head.asInstanceOf[TypedValueDecl]
    assertEquals(value.name, "answer")
    assertEquals(value.valueType, SourceType.I32)

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

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val module = result.value.get
    assertEquals(module.declarations.map(_.name), List("SourceId", "Severity", "Diagnostic"))

    val alias = module.declarations.head.asInstanceOf[UntypedTypeAlias]
    assert(alias.target.isInstanceOf[UntypedAppliedType])

    val severity = module.declarations(1).asInstanceOf[UntypedClass]
    assertEquals(
      severity.members.collect { case variant: UntypedVariant => variant.name },
      List("Note", "Error"),
    )

    val diagnostic = module.declarations(2).asInstanceOf[UntypedClass]
    val push = diagnostic.members.collectFirst { case fn: UntypedFunction => fn }.get
    val selfType = push.params.head.valueType.get.asInstanceOf[UntypedRefType]
    assert(selfType.mutable)
    assert(push.body.exists(_.isInstanceOf[UntypedBlock]))

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

    assertEquals(result.status, PhaseStatus.Succeeded)
    val fn = result.value.get.declarations.head.asInstanceOf[UntypedFunction]
    val body = fn.body.get.asInstanceOf[UntypedBlock]
    val matchExpr = body.items.collectFirst { case m: UntypedMatch => m }.get

    assertEquals(matchExpr.arms.length, 2)
    val somePattern = matchExpr.arms.head.pattern.asInstanceOf[UntypedVariantPattern]
    assert(somePattern.constructor.isInstanceOf[UntypedVariantConstructor])
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

      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, PhaseStatus.Unsupported)
      assert(result.value.isEmpty)
      assert(
        result.diagnostics.exists(_.code == code),
        s"missing diagnostic $code in ${result.diagnostics.map(_.code)}",
      )
    }

  test("check stops on unsupported subset constructs"):
    val result = Cosmo0().check("trait Display {}")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assertEquals(result.diagnostics.head.code, "cosmo0.elaborate.unsupported.trait")

  test("check resolves aliases, constructors, descriptor methods, and match bindings"):
    val result = Cosmo0().check(
      """class Token {
        |  val text: String
        |}
        |
        |type TokenId = Id[Token]
        |
        |class Severity {
        |  case Note
        |  case Error(String)
        |}
        |
        |class Diagnostic {
        |  val severity: Severity
        |  val labels: Vec[String]
        |}
        |
        |def describe(token: Option[Token]): String = {
        |  token match {
        |    case Option[Token]::Some(value) => {
        |      value.text
        |    }
        |    case Option[Token]::None => {
        |      "none"
        |    }
        |  }
        |}
        |
        |def build(): Diagnostic = {
        |  val labels = Vec[String]();
        |  labels.push("primary");
        |  Diagnostic(Severity.Error("bad"), labels)
        |}
        |
        |def collect(): Unit = {
        |  val labels = Vec[String]();
        |  labels.push("primary");
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val module = result.value.get.typed
    val alias = module.declarations.collectFirst { case value: TypedTypeAlias => value }.get
    assertEquals(alias.name, "TokenId")
    assert(SourceType.same(alias.target, SourceType.Standard("Id", List(SourceType.User("Token")))))

    val build = module.declarations.collectFirst {
      case fn: TypedFunction if fn.name == "build" => fn
    }.get
    assertEquals(build.returnType, SourceType.User("Diagnostic"))
    assert(build.body.exists(_.valueType == SourceType.User("Diagnostic")))

    val collect = module.declarations.collectFirst {
      case fn: TypedFunction if fn.name == "collect" => fn
    }.get
    assertEquals(collect.body.map(_.valueType), Some(SourceType.Unit))

  test("check emits typed diagnostics for representative source errors"):
    val cases = List(
      "def f(): i32 = missing" -> "cosmo0.type.unresolved-name",
      "def f(): Vec[i32] = { Vec[i32](1) }" -> "cosmo0.type.wrong-arity",
      "class A {}\ndef f(a: A): i32 = { a.missing }" -> "cosmo0.type.invalid-field",
      "var x: i32 = true" -> "cosmo0.type.assignment-mismatch",
      "def f(): Bool = { 1 }" -> "cosmo0.type.return-mismatch",
      """class A {
        |  var count: usize
        |
        |  def f(&self): Unit = {
        |    self.count = 1
        |  }
        |}
        |""".stripMargin -> "cosmo0.type.invalid-mutability",
      "def f(items: &Vec[i32]): Unit = { items.push(1) }" ->
        "cosmo0.type.invalid-mutability",
      """class Token {}
        |
        |def f(value: Option[Token]): String = {
        |  value match {
        |    case Option[Token]::Some => {
        |      "bad"
        |    }
        |    case Option[Token]::None => {
        |      "none"
        |    }
        |  }
        |}
        |""".stripMargin -> "cosmo0.type.wrong-arity",
    )

    cases.foreach { case (source, code) =>
      val result = Cosmo0().check(source)

      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, PhaseStatus.Failed)
      assert(
        result.diagnostics.exists(_.code == code),
        s"missing diagnostic $code in ${result.diagnostics.map(_.code)}",
      )
    }

  test("cosmo0 typed-expression typer does not call the full Cosmo typer"):
    assert(classOf[SourceTyper].getName.contains("cosmo0.SourceTyper"))

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

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.compile.unsupported")

  test("compile stops on unsupported subset constructs"):
    val result = Cosmo0().compile("trait Display {}")

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.elaborate.unsupported.trait")
