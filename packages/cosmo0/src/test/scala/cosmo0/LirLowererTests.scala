package cosmo0

class LirLowererTests extends munit.FunSuite:
  test("lowers typed declarations and ordinary function bodies to checked LIR"):
    val result = Cosmo0().lower(
      """type Count = i32
        |
        |class Counter {
        |  var count: i32
        |
        |  def get(&self): i32 = {
        |    self.count
        |  }
        |
        |  def set(&mut self, value: i32): Unit = {
        |    self.count = value
        |  }
        |}
        |
        |def id(value: i32): i32 = {
        |  return value
        |}
        |
        |def read(counter: &Counter): i32 = {
        |  counter.get()
        |}
        |
        |def replace(value: i32): i32 = {
        |  var current: i32 = 0;
        |  current = id(value);
        |  current
        |}
        |
        |def write(counter: &mut Counter, value: i32): Unit = {
        |  counter.set(value)
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    assert(result.diagnostics.isEmpty)

    assertEquals(
      LirDebugRenderer.renderModule(result.value.get.lir),
      """module memory {
        |  typealias @Count Count = i32
        |
        |  type @Counter Counter {
        |    field var count: i32
        |  }
        |
        |  fn @Counter.get get(%self self: &Counter) -> i32 owner @Counter {
        |    local %tmp0 tmp0: i32
        |
        |    ^entry:
        |      %tmp0 = field_get %self.count: i32
        |      return %tmp0
        |  }
        |
        |  fn @Counter.set set(%self self: &mut Counter, %value value: i32) -> Unit owner @Counter {
        |    ^entry:
        |      field_set %self.count = %value
        |      return
        |  }
        |
        |  fn @id id(%value value: i32) -> i32 {
        |    ^entry:
        |      return %value
        |  }
        |
        |  fn @read read(%counter counter: &Counter) -> i32 {
        |    local %tmp0 tmp0: i32
        |
        |    ^entry:
        |      %tmp0 = method_call %counter.get() -> i32
        |      return %tmp0
        |  }
        |
        |  fn @replace replace(%value value: i32) -> i32 {
        |    local %current current: i32 mut
        |    local %tmp0 tmp0: i32
        |
        |    ^entry:
        |      alloc %current current: i32 mut = 0:i32
        |      %tmp0 = call @id(%value) -> i32
        |      assign %current = %tmp0
        |      return %current
        |  }
        |
        |  fn @write write(%counter counter: &mut Counter, %value value: i32) -> Unit {
        |    ^entry:
        |      method_call %counter.set(%value) -> Unit
        |      return
        |  }
        |}""".stripMargin,
    )

  test("lower stops on source typing errors before LIR emission"):
    val result = Cosmo0().lower("def f(): i32 = true")

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.type.return-mismatch"),
      s"missing source typing diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("lower reports unsupported typed constructs with source spans"):
    val result = Cosmo0().lower(
      """def choose(flag: Bool): i32 = {
        |  if (flag) {
        |    1
        |  } else {
        |    2
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.lir.lower.unsupported-expression"),
      s"missing lowering diagnostic in ${result.diagnostics.map(_.code)}",
    )
    assert(result.diagnostics.exists(_.span.nonEmpty))

  test("lowered output is checked before backend emission"):
    val source = SourceFile("<typed>", "")
    val span = source.span(0, 0)
    val idSignature = CallableSignature(
      "id",
      List(CallableParam("value", SourceType.I32, span)),
      SourceType.I32,
    )
    val idFunction = TypedFunction(
      "id",
      List(TypedParam("value", SourceType.I32, None, span)),
      SourceType.I32,
      Some(TypedName(UntypedPath(List("value"), span), SourceType.I32, false, true, span)),
      idSignature,
      None,
      span,
    )
    val badFunction = TypedFunction(
      "bad",
      Nil,
      SourceType.I32,
      Some(
        TypedCall(
          TypedName(UntypedPath(List("id"), span), idSignature.functionType, false, false, span),
          List(TypedBoolLiteral(true, SourceType.Bool, span)),
          SourceType.I32,
          idSignature,
          span,
        ),
      ),
      CallableSignature("bad", Nil, SourceType.I32),
      None,
      span,
    )
    val module = TypedModule(source, List(idFunction, badFunction), span)

    val result = LirLowerer().lower(module)

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.lir.assignment-mismatch"),
      s"missing LIR checker diagnostic in ${result.diagnostics.map(_.code)}",
    )
