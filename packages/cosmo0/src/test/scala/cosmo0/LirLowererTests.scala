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
        |    structured:
        |      %tmp0 = field_get %self.count: i32
        |      return %tmp0
        |  }
        |
        |  fn @Counter.set set(%self self: &mut Counter, %value value: i32) -> Unit owner @Counter {
        |    structured:
        |      field_set %self.count = %value
        |      return
        |  }
        |
        |  fn @id id(%value value: i32) -> i32 {
        |    structured:
        |      return %value
        |  }
        |
        |  fn @read read(%counter counter: &Counter) -> i32 {
        |    local %tmp0 tmp0: i32
        |
        |    structured:
        |      %tmp0 = method_call %counter.get() -> i32
        |      return %tmp0
        |  }
        |
        |  fn @replace replace(%value value: i32) -> i32 {
        |    local %current current: i32 mut
        |    local %tmp0 tmp0: i32
        |
        |    structured:
        |      alloc %current current: i32 mut = 0:i32
        |      %tmp0 = call @id(%value) -> i32
        |      assign %current = %tmp0
        |      return %current
        |  }
        |
        |  fn @write write(%counter counter: &mut Counter, %value value: i32) -> Unit {
        |    structured:
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

  test(
    "lower reports unsupported non-control typed constructs with source spans",
  ):
    val result = Cosmo0().lower(
      """def bump(): Unit = {
        |  var count: i32 = 0;
        |  count += 1
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.lir.lower.unsupported-expression",
      ),
      s"missing lowering diagnostic in ${result.diagnostics.map(_.code)}",
    )
    assert(result.diagnostics.exists(_.span.nonEmpty))

  test("lowers if expressions into structured LIR branches"):
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
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("structured:"))
    assert(rendered.contains("if %flag {"))
    assert(rendered.contains("assign %tmp0 = 1:i32"))
    assert(rendered.contains("assign %tmp0 = 2:i32"))
    assert(rendered.contains("return %tmp0"))
    assert(!rendered.contains("cond_branch"))
    assert(!rendered.contains("^if0_"))

  test("lowers boolean and/or with structured short-circuit control flow"):
    val result = Cosmo0().lower(
      """def both(left: Bool, right: Bool): Bool = {
        |  left and right
        |}
        |
        |def either(left: Bool, right: Bool): Bool = {
        |  left or right
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("structured:"))
    assert(rendered.contains("if %left {"))
    assert(rendered.contains("assign %tmp0 = %right"))
    assert(rendered.contains("assign %tmp0 = false"))
    assert(rendered.contains("assign %tmp0 = true"))
    assert(!rendered.contains("cond_branch"))
    assert(!rendered.contains("^and0_"))
    assert(!rendered.contains("^or0_"))
    assert(!rendered.contains("descriptor Bool::and("))
    assert(!rendered.contains("descriptor Bool::or("))

  test("lowers while, loop, break, continue, and descriptor-backed for loops"):
    val result = Cosmo0().lower(
      """def spin(keep: Bool): Unit = {
        |  while (keep) {
        |    continue
        |  }
        |}
        |
        |def once(): Unit = {
        |  loop {
        |    break
        |  }
        |}
        |
        |def each(items: Vec[i32]): Unit = {
        |  for (item in items) {
        |    item;
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("structured:"))
    assert(rendered.contains("condition %keep"))
    assert(rendered.contains("continue"))
    assert(rendered.contains("condition always"))
    assert(rendered.contains("break"))
    assert(
      rendered.contains(
        "%tmp0 = descriptor Vec[i32]::iter_has_next(%items) -> Bool",
      ),
    )
    assert(
      rendered.contains("%item = descriptor Vec[i32]::iter_next(%items) -> i32"),
    )
    assert(!rendered.contains("^while0_"))
    assert(!rendered.contains("^loop0_"))

  test(
    "lowers descriptor constructors and standard generic methods to intrinsics",
  ):
    val result = Cosmo0().lower(
      """class Node {}
        |
        |def use(
        |  set: Set[i32],
        |  map: Map[String, i32],
        |  arena: Arena[Node],
        |  fallback: Node
        |): Unit = {
        |  val local = Vec[i32]();
        |  local.push(1);
        |  local.set(0, 2);
        |  val item = local.get(0);
        |  val count = local.size();
        |  val length = local.len();
        |  val previous = map.get("key");
        |  map.insert("key", item);
        |  set.insert(item);
        |  val seen = set.contains(item);
        |  val node_id = arena.alloc(fallback);
        |  val node = arena.get(node_id);
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("descriptor Vec[i32]::<init>() -> Vec[i32]"))
    assert(
      rendered.contains("descriptor Vec[i32]::push(%local, 1:i32) -> Unit"),
    )
    assert(
      rendered.contains(
        "descriptor Vec[i32]::set(%local, 0:usize, 2:i32) -> Unit",
      ),
    )
    assert(
      rendered.contains("descriptor Vec[i32]::get(%local, 0:usize) -> i32"),
    )
    assert(rendered.contains("descriptor Vec[i32]::size(%local) -> usize"))
    assert(rendered.contains("descriptor Vec[i32]::len(%local) -> usize"))
    assert(
      rendered.contains(
        "descriptor Map[String, i32]::get(%map, \"key\") -> Option[i32]",
      ),
    )
    assert(
      rendered.contains(
        "descriptor Map[String, i32]::insert(%map, \"key\", %item) -> Option[i32]",
      ),
    )
    assert(
      rendered.contains("descriptor Set[i32]::insert(%set, %item) -> Unit"),
    )
    assert(
      rendered.contains("descriptor Set[i32]::contains(%set, %item) -> Bool"),
    )
    assert(
      rendered.contains(
        "descriptor Arena[Node]::alloc(%arena, %fallback) -> Id[Node]",
      ),
    )
    assert(
      rendered.contains(
        "descriptor Arena[Node]::get(%arena, %node_id) -> &Node",
      ),
    )

  test("lowers trusted extern std declarations to direct extern calls"):
    val result = Cosmo0().lower(
      """def println(value: String): Unit
        |
        |def smoke(): Unit = {
        |  println("cosmo1 extern smoke")
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(
      rendered.contains(
        "fn @println println(%value value: String) -> Unit extern cosmo0.extern.v0 \"::cosmo0_runtime::println\"",
      ),
    )
    assert(rendered.contains("call @println(\"cosmo1 extern smoke\") -> Unit"))
    assert(!rendered.contains("descriptor Runtime"))

  test("lowers direct C extern declarations to fixed-arity calls"):
    val result = Cosmo0().lower(
      """@include("stdlib.h");
        |@extern("c", name = "abs")
        |def c_abs(value: i32): i32
        |
        |def use(value: i32): i32 = {
        |  c_abs(value)
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assertEquals(
      result.value.get.lir.cIncludes.map(_.header),
      List("<stdlib.h>"),
    )
    assert(
      rendered.contains(
        "fn @c_abs c_abs(%value value: i32) -> i32 extern c \"abs\"",
      ),
    )
    assert(rendered.contains("call @c_abs(%value) -> i32"))
    assert(!rendered.contains("descriptor Runtime"))

  test(
    "lower reports missing extern metadata for untrusted bodyless declarations",
  ):
    val result = Cosmo0().lower("def host_call(value: String): Unit")

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.lir.lower.missing-extern-binding",
      ),
      s"missing extern binding diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("lowers primitive scalar operations, boolean branches, and references"):
    val result = Cosmo0().lower(
      """class Cell {
        |  var value: usize
        |}
        |
        |def read_cell(cell: &Cell): usize = {
        |  cell.value
        |}
        |
        |def write_cell(cell: &mut Cell, value: usize): Unit = {
        |  cell.value = value
        |}
        |
        |def primitive_ops(flag: Bool, left: i32, right: i32, text: String, byte: u8): Bool = {
        |  val sum = left + right;
        |  val has_text = text.size() > 0;
        |  val same_text = text == "ok";
        |  if (flag and sum >= 0) {
        |    has_text and byte != 0
        |  } else {
        |    same_text or text.len() == 0
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("%tmp0 = field_get %cell.value: usize"))
    assert(rendered.contains("field_set %cell.value = %value"))
    assert(rendered.contains("descriptor i32::add(%left, %right) -> i32"))
    assert(rendered.contains("descriptor i32::ge(%sum, 0:i32) -> Bool"))
    assert(rendered.contains("descriptor String::size(%text) -> usize"))
    assert(rendered.contains("descriptor String::eq(%text, \"ok\") -> Bool"))
    assert(rendered.contains("descriptor String::len(%text) -> usize"))
    assert(rendered.contains("descriptor u8::ne(%byte, 0:u8) -> Bool"))
    assert(rendered.contains("descriptor usize::gt("))
    assert(rendered.contains("descriptor usize::eq("))
    assert(rendered.contains("if %flag {"))
    assert(!rendered.contains("cond_branch"))
    assert(!rendered.contains("descriptor Bool::and("))
    assert(!rendered.contains("descriptor Bool::or("))

  test("lowers ascii and rune literals to u8 and u32 integer values"):
    val result = Cosmo0().lower(
      """def ascii_left_paren(): u8 = {
        |  a"("
        |}
        |
        |def rune_left_paren(): u32 = {
        |  c"("
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("40:u8"))
    assert(rendered.contains("40:u32"))

  test(
    "lowers map and set for loops through deterministic descriptor iteration",
  ):
    val result = Cosmo0().lower(
      """def walk(values: Map[String, i32], seen: Set[String]): Unit = {
        |  for (key in values) {
        |    key;
        |  }
        |  for (item in seen) {
        |    item;
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(
      rendered.contains(
        "descriptor Map[String, i32]::iter_has_next(%values) -> Bool",
      ),
    )
    assert(
      rendered.contains(
        "descriptor Map[String, i32]::iter_next(%values) -> String",
      ),
    )
    assert(
      rendered.contains("descriptor Set[String]::iter_has_next(%seen) -> Bool"),
    )
    assert(
      rendered.contains("descriptor Set[String]::iter_next(%seen) -> String"),
    )

  test(
    "lowers variant construction and match expressions into tag checks and payload reads",
  ):
    val result = Cosmo0().lower(
      """def some(value: i32): Option[i32] = {
        |  Option[i32]::Some(value)
        |}
        |
        |def none(): Option[i32] = {
        |  Option[i32]::None
        |}
        |
        |def unpack(value: Option[i32]): i32 = {
        |  value match {
        |    case Option[i32]::Some(item) => {
        |      item
        |    }
        |    case Option[i32]::None => {
        |      0
        |    }
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("%tmp0 = variant Option[i32]::Some(%value)"))
    assert(rendered.contains("%tmp0 = variant Option[i32]::None()"))
    assert(rendered.contains("variant_match %value: Option[i32] {"))
    assert(rendered.contains("case Some {"))
    assert(rendered.contains("case None {"))
    assert(rendered.contains("%item = variant_payload %value.Some[0]: i32"))
    assert(rendered.contains("return %tmp0"))
    assert(!rendered.contains("variant_is %value"))
    assert(!rendered.contains("^match"))

  test("lowers supported user variant matches into structured LIR"):
    val result = Cosmo0().lower(
      """class Shape {
        |  case Empty
        |  case Circle(i32)
        |  case Rect(i32, i32)
        |}
        |
        |def score(shape: Shape): i32 = {
        |  shape match {
        |    case Shape.Circle(radius) => {
        |      radius
        |    }
        |    case Shape.Rect(width, height) => {
        |      width + height
        |    }
        |    case Shape.Empty => {
        |      0
        |    }
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("structured:"))
    assert(rendered.contains("variant_match %shape: Shape {"))
    assert(rendered.contains("case Circle {"))
    assert(rendered.contains("case Rect {"))
    assert(rendered.contains("case Empty {"))
    assert(rendered.contains("%radius = variant_payload %shape.Circle[0]: i32"))
    assert(!rendered.contains("variant_is %shape"))
    assert(!rendered.contains("^match"))

  test("lowers minimal Result variants and Vec emptiness checks"):
    val result = Cosmo0().lower(
      """class Diagnostic {
        |  val message: String
        |}
        |
        |def first(items: Vec[i32]): Result[i32, Diagnostic] = {
        |  if (items.is_empty()) {
        |    Result[i32, Diagnostic]::Err(Diagnostic("empty"))
        |  } else {
        |    Result[i32, Diagnostic]::Ok(items.get(0))
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"lowering failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(result.value.get.lir)
    assert(rendered.contains("descriptor Vec[i32]::is_empty(%items) -> Bool"))
    assert(
      rendered.contains("descriptor Vec[i32]::get(%items, 0:usize) -> i32"),
    )
    assert(rendered.contains("variant Result[i32, Diagnostic]::Ok"))
    assert(rendered.contains("variant Result[i32, Diagnostic]::Err"))

  test("lower reports invalid structured control-flow inputs"):
    val invalidBreak = Cosmo0().lower("def bad(): Unit = { break }")
    assertEquals(invalidBreak.phase, Phase.Compile)
    assertEquals(invalidBreak.status, PhaseStatus.Failed)
    assert(
      invalidBreak.diagnostics.exists(
        _.code == "cosmo0.lir.lower.invalid-break",
      ),
      s"missing invalid break diagnostic in ${invalidBreak.diagnostics.map(_.code)}",
    )

    val branchMismatch = Cosmo0().lower(
      """def bad(flag: Bool): i32 = {
        |  if (flag) {
        |    1
        |  } else {
        |    true
        |  }
        |}
        |""".stripMargin,
    )
    assertEquals(branchMismatch.status, PhaseStatus.Failed)
    assert(
      branchMismatch.diagnostics.exists(
        _.code == "cosmo0.type.branch-mismatch",
      ),
      s"missing branch mismatch diagnostic in ${branchMismatch.diagnostics.map(_.code)}",
    )

    val invalidPayload = Cosmo0().lower(
      """def bad(value: Option[i32]): i32 = {
        |  value match {
        |    case Option[i32]::Some(true) => {
        |      1
        |    }
        |    case Option[i32]::None => {
        |      0
        |    }
        |  }
        |}
        |""".stripMargin,
    )
    assertEquals(invalidPayload.status, PhaseStatus.Failed)
    assert(
      invalidPayload.diagnostics.exists(
        _.code == "cosmo0.type.invalid-match-payload",
      ),
      s"missing invalid payload diagnostic in ${invalidPayload.diagnostics.map(_.code)}",
    )

  test("lower reports unsupported descriptor operations that reach lowering"):
    val source = SourceFile("<typed>", "")
    val span = source.span(0, 0)
    val idType = SourceType.Standard("Id", List(SourceType.I32))
    val badFunction = TypedFunction(
      "bad",
      Nil,
      idType,
      Some(
        TypedCall(
          TypedTypeConstructorExpr(
            idType,
            SourceType.Function(Nil, idType),
            span,
          ),
          Nil,
          idType,
          CallableSignature("<init>", Nil, idType),
          span,
        ),
      ),
      CallableSignature("bad", Nil, idType),
      None,
      span,
    )
    val module = TypedModule(source, List(badFunction), span)

    val result = LirLowerer(module).lower()

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(
        _.code == "cosmo0.lir.lower.unsupported-descriptor",
      ),
      s"missing unsupported descriptor diagnostic in ${result.diagnostics.map(_.code)}",
    )

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
      Some(
        TypedName(
          UntypedPath(List("value"), span),
          SourceType.I32,
          false,
          true,
          span,
        ),
      ),
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
          TypedName(
            UntypedPath(List("id"), span),
            idSignature.functionType,
            false,
            false,
            span,
          ),
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

    val result = LirLowerer(module).lower()

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.lir.assignment-mismatch"),
      s"missing LIR checker diagnostic in ${result.diagnostics.map(_.code)}",
    )
