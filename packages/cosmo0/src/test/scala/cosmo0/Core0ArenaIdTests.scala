package cosmo0

class Core0ArenaIdTests extends munit.FunSuite:
  test("Arena allocation, lookup, mutable lookup, and length type-check"):
    val result = Cosmo0().check(
      """class Expr {
        |  var text: String
        |}
        |
        |type ExprId = Id[Expr]
        |
        |class ExprStore {
        |  var exprs: Arena[Expr]
        |
        |  def add(&mut self, expr: Expr): ExprId = {
        |    self.exprs.alloc(expr)
        |  }
        |
        |  def first_text(&self, id: ExprId): String = {
        |    val expr = self.exprs.get(id);
        |    expr.text
        |  }
        |
        |  def rename(&mut self, id: ExprId, text: String): Unit = {
        |    val expr = self.exprs.get_mut(id);
        |    expr.text = text
        |  }
        |
        |  def count(&self): usize = {
        |    self.exprs.len()
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assert(
      result.isSuccess,
      s"core0.arena-id check failed with diagnostics: ${result.diagnostics
          .map(d => d.code -> d.message)}",
    )

  test("Arena APIs lower to typed descriptor operations"):
    val result = Cosmo0().lower(
      """class Expr {
        |  var text: String
        |}
        |
        |def use(arena: Arena[Expr], expr: Expr): Unit = {
        |  val id = arena.alloc(expr);
        |  val current = arena.get(id);
        |  val editable = arena.get_mut(id);
        |  val count = arena.len();
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"core0.arena-id lowering failed with diagnostics: ${result.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(
      rendered.contains(
        "descriptor Arena[Expr]::alloc(%arena, %expr) -> Id[Expr]",
      ),
    )
    assert(
      rendered.contains("descriptor Arena[Expr]::get(%arena, %id) -> &Expr"),
    )
    assert(
      rendered.contains(
        "descriptor Arena[Expr]::get_mut(%arena, %id) -> &mut Expr",
      ),
    )
    assert(rendered.contains("descriptor Arena[Expr]::len(%arena) -> usize"))

  test("typed IDs reject mixing across arena item types"):
    val result = Cosmo0().check(
      """class Expr {}
        |class Ty {}
        |
        |def invalid(exprs: Arena[Expr], tys: Arena[Ty], expr: Expr, ty: Ty): Unit = {
        |  val expr_id = exprs.alloc(expr);
        |  val ty_id = tys.alloc(ty);
        |  exprs.get(ty_id);
        |  tys.get(expr_id)
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(diagnostic =>
        diagnostic.code == "cosmo0.type.invalid-call" &&
          diagnostic.message.contains("expected Id[Expr]"),
      ),
      s"missing typed id diagnostic in ${result.diagnostics.map(d => d.code -> d.message)}",
    )

  test("get_mut requires a mutable arena receiver"):
    val result = Cosmo0().check(
      """class Expr {}
        |
        |def invalid(exprs: &Arena[Expr], id: Id[Expr]): Unit = {
        |  exprs.get_mut(id)
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.type.invalid-mutability"),
      s"missing mutable receiver diagnostic in ${result.diagnostics
          .map(d => d.code -> d.message)}",
    )

  test("cosmo1 syntax AST arena source checks and compiles"):
    val spanSource =
      PackageNodeFs
        .readFileSync("packages/cosmoc/src/source/span.cos", "utf8")
        .asInstanceOf[String]
    val astSource =
      PackageNodeFs
        .readFileSync("packages/cosmoc/src/syntax/ast.cos", "utf8")
        .asInstanceOf[String]

    val result = Cosmo0().compile(
      SourceFile(
        "packages/cosmoc/src/syntax/ast.cos",
        List(spanSource, astSource)
          .map(
            _.linesIterator.filterNot(_.startsWith("import ")).mkString("\n"),
          )
          .mkString("\n"),
      ),
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"cosmo1 syntax AST arena compile failed with diagnostics: ${result.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assert(
      result.value.get.output.contains("cosmo0_runtime::Arena<SyntaxExpr>"),
    )
    assert(
      result.value.get.output.contains("inline bool syntax_ast_arena_smoke()"),
    )
    assert(
      result.value.get.output
        .contains("inline std::string syntax_debug_module("),
    )
    assert(
      result.value.get.output
        .contains("inline Span syntax_diagnostic_span_for_expr("),
    )
