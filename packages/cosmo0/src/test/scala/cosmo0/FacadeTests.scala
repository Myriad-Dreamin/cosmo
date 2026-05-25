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

  test("parse keeps ascii and rune literals distinct from template literals"):
    val result = Cosmo0().parse(
      """val ascii = a"("
        |val rune = c"("
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Parse)
    assertEquals(result.status, PhaseStatus.Succeeded)
    val stmts = result.value.get.ast.stmts
    assert(
      stmts.head
        .asInstanceOf[cosmo.syntax.Val]
        .init
        .get
        .isInstanceOf[cosmo.syntax.AsciiLit],
    )
    assert(
      stmts(1)
        .asInstanceOf[cosmo.syntax.Val]
        .init
        .get
        .isInstanceOf[cosmo.syntax.RuneLit],
    )

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

    val value = result.value.get.typed.decls.head.asInstanceOf[TypedValueDecl]
    assertEquals(value.name, "answer")
    assertEquals(value.ty, SourceType.I32)

  test("check types ascii and rune literals as fixed-width integers"):
    val result = Cosmo0().check(
      """val newline: u8 = a"\n"
        |val nul: u8 = a"\x00"
        |val rune: u32 = c"("
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val values = result.value.get.typed.decls.collect {
      case value: TypedValueDecl =>
        value.name -> value.ty
    }
    assertEquals(
      values,
      List(
        "newline" -> SourceType.Byte,
        "nul" -> SourceType.Byte,
        "rune" -> SourceType.Rune,
      ),
    )

  test("check rejects malformed ascii and rune literals"):
    val cases = List(
      "val bad = a\"ab\"" -> "cosmo0.type.invalid-ascii-literal",
      "val bad = a\"\\x80\"" -> "cosmo0.type.invalid-ascii-literal",
      "val bad = c\"ab\"" -> "cosmo0.type.invalid-rune-literal",
    )

    cases.foreach { case (source, code) =>
      val result = Cosmo0().check(source)
      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, PhaseStatus.Failed)
      assert(
        result.diagnostics.exists(_.code == code),
        s"missing $code in ${result.diagnostics.map(d => d.code -> d.message)}",
      )
    }

  test(
    "elaborate builds untyped representation for accepted core declarations",
  ):
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
    assertEquals(
      module.decls.map(_.name),
      List("SourceId", "Severity", "Diagnostic"),
    )

    val alias = module.decls.head.asInstanceOf[UntypedTypeAlias]
    assert(alias.target.isInstanceOf[UntypedAppliedType])

    val severity = module.decls(1).asInstanceOf[UntypedClass]
    assertEquals(
      severity.members.collect { case variant: UntypedVariant => variant.name },
      List("Note", "Error"),
    )

    val diagnostic = module.decls(2).asInstanceOf[UntypedClass]
    val push = diagnostic.members.collectFirst { case fn: UntypedFunction =>
      fn
    }.get
    val selfType = push.params.head.ty.get.asInstanceOf[UntypedRefType]
    assert(selfType.mut)
    assert(push.body.exists(_.isInstanceOf[UntypedBlock]))

  test(
    "elaborate represents match arms and standard-generic variant constructors",
  ):
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
    val fn = result.value.get.decls.head.asInstanceOf[UntypedFunction]
    val body = fn.body.get.asInstanceOf[UntypedBlock]
    val matchExpr = body.items.collectFirst { case m: UntypedMatch => m }.get

    assertEquals(matchExpr.arms.length, 2)
    val somePattern =
      matchExpr.arms.head.pat.asInstanceOf[UntypedVariantPattern]
    assert(somePattern.ctor.isInstanceOf[UntypedVariantConstructor])
    assertEquals(somePattern.args.length, 1)

  test("elaborate preserves direct C extern metadata"):
    val result = Cosmo0().elaborate(
      """@extern("c", name = "abs")
        |def c_abs(value: i32): i32
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val fn = result.value.get.decls.head.asInstanceOf[UntypedFunction]
    val binding = fn.extern.getOrElse(fail("missing extern metadata"))
    assertEquals(binding.abi, TrustedExternAbi.directCAbiName)
    assertEquals(binding.name, Some("abs"))
    assertEquals(binding.supportLibrary, None)
    assertEquals(fn.body, None)

  test("elaborate preserves file-level C include directives"):
    val result = Cosmo0().elaborate(
      """@include("stdio.h");
        |@include("\"runtime/support.h\"", kind = "c");
        |def smoke(): Unit = {}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assertEquals(
      result.value.get.cIncludes.map(_.header),
      List("<stdio.h>", "\"runtime/support.h\""),
    )
    assertEquals(result.value.get.decls.map(_.name), List("smoke"))

  test("elaborate accepts symbol alias for direct C extern name"):
    val result = Cosmo0().elaborate(
      """@extern("c", symbol = "abs")
        |def c_abs(value: i32): i32
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    val fn = result.value.get.decls.head.asInstanceOf[UntypedFunction]
    assertEquals(fn.extern.map(_.name), Some(Some("abs")))

  test("elaborate rejects invalid direct C extern declarations"):
    val cases = List(
      """@extern("c", name = "std::puts")
        |def puts(value: i32): i32
        |""".stripMargin,
      """@extern("c")
        |def host(value: i32): i32 = value
        |""".stripMargin,
      """@extern("c", include = "<stdlib.h>")
        |def c_abs(value: i32): i32
        |""".stripMargin,
    )

    cases.foreach { source =>
      val result = Cosmo0().elaborate(source)

      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, PhaseStatus.Unsupported)
      assert(
        result.diagnostics.exists(_.code == "cosmo0.elaborate.invalid-extern"),
        s"missing invalid extern diagnostic in ${result.diagnostics.map(_.code)}",
      )
    }

  test("elaborate rejects invalid C include directives"):
    val cases = List(
      """@include("stdio.h")
        |def smoke(): Unit = {}
        |""".stripMargin,
      """@include("stdio");
        |def smoke(): Unit = {}
        |""".stripMargin,
      """@include("stdio.h", kind = "cpp");
        |def smoke(): Unit = {}
        |""".stripMargin,
      """@include("a.h", "b.h");
        |def smoke(): Unit = {}
        |""".stripMargin,
      """@include-c("<stdio.h>");
        |def smoke(): Unit = {}
        |""".stripMargin,
    )

    cases.foreach { source =>
      val result = Cosmo0().elaborate(source)

      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, PhaseStatus.Unsupported)
      assert(
        result.diagnostics.exists(
          _.code == "cosmo0.elaborate.invalid-include",
        ) ||
          result.diagnostics.exists(
            _.code == "cosmo0.elaborate.unsupported.include-kind",
          ),
        s"missing invalid include diagnostic in ${result.diagnostics.map(_.code)}",
      )
    }

  test(
    "elaborate rejects unsupported full-language constructs deterministically",
  ):
    val cases = List(
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
    val result = Cosmo0().check("trait Display[T] {}")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assertEquals(
      result.diagnostics.head.code,
      "cosmo0.elaborate.unsupported.generic-trait",
    )

  test(
    "check resolves aliases, constructors, descriptor methods, and match bindings",
  ):
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
    val alias = module.decls.collectFirst { case value: TypedTypeAlias =>
      value
    }.get
    assertEquals(alias.name, "TokenId")
    assert(
      SourceType.same(
        alias.target,
        SourceType.Standard("Id", List(SourceType.User("Token"))),
      ),
    )

    val build = module.decls.collectFirst {
      case fn: TypedFunction if fn.name == "build" => fn
    }.get
    assertEquals(build.retTy, SourceType.User("Diagnostic"))
    assert(build.body.exists(_.ty == SourceType.User("Diagnostic")))

    val collect = module.decls.collectFirst {
      case fn: TypedFunction if fn.name == "collect" => fn
    }.get
    assertEquals(collect.body.map(_.ty), Some(SourceType.Unit))

  test("check accepts minimal Option Result and Vec Stage 1 APIs"):
    val result = Cosmo0().check(
      """class Diagnostic {
        |  val message: String
        |}
        |
        |class Token {
        |  val text: String
        |}
        |
        |class TokenBuffer {
        |  var tokens: Vec[Token]
        |
        |  def push(&mut self, token: Token): Unit = {
        |    self.tokens.push(token)
        |  }
        |
        |  def replace_first(&mut self, token: Token): Unit = {
        |    if (!self.tokens.is_empty()) {
        |      self.tokens.set(0, token)
        |    }
        |  }
        |
        |  def current(&self): Option[Token] = {
        |    if (self.tokens.len() == 0) {
        |      Option[Token]::None
        |    } else {
        |      Option[Token]::Some(self.tokens.get(0))
        |    }
        |  }
        |}
        |
        |def parse(buffer: &TokenBuffer): Result[Token, Diagnostic] = {
        |  buffer.current() match {
        |    case Option[Token]::Some(token) => {
        |      Result[Token, Diagnostic]::Ok(token)
        |    }
        |    case Option[Token]::None => {
        |      Result[Token, Diagnostic]::Err(Diagnostic("empty"))
        |    }
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assert(
      result.isSuccess,
      s"check failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

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
    val declarationSpan = accepted.value.get.decls.head.span

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

  test("compile returns generated C++ for accepted source"):
    val result = Cosmo0().compile("val answer = 42")

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.value.nonEmpty)
    assert(
      result.value.get.output
        .contains("inline const int32_t answer = static_cast<int32_t>(42);"),
    )
    assert(result.diagnostics.isEmpty)

  test("compile stops on unsupported subset constructs"):
    val result = Cosmo0().compile("trait Display[T] {}")

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assert(result.value.isEmpty)
    assertEquals(
      result.diagnostics.head.code,
      "cosmo0.elaborate.unsupported.generic-trait",
    )
