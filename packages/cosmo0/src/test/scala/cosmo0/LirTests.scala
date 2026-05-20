package cosmo0

class LirTests extends munit.FunSuite:
  private val tokenType = SourceType.User("Token")
  private val optionTokenType = SourceType.Standard("Option", List(tokenType))
  private val stringVecType = SourceType.Standard("Vec", List(SourceType.String))

  test("debug renderer covers LIR operations and explicit terminators"):
    val rendered = LirDebugRenderer.renderFunction(processFunction())

    assertEquals(
      rendered,
      """fn @process process(%input input: Token) -> i32 {
        |  local %count count: i32 mut
        |  local %is_some is_some: Bool
        |  local %labels labels: Vec[String] mut
        |  local %maybe maybe: Option[Token]
        |  local %payload payload: Token
        |  local %tag tag: i32
        |  local %tmp tmp: usize
        |  local %token token: Token mut
        |
        |  ^dead:
        |    unreachable "not scheduled"
        |  ^dispatch:
        |    branch ^entry
        |  ^entry:
        |    alloc %count count: i32 mut = 0:i32
        |    assign %count = 1:i32
        |    %tmp = field_get %token.offset: usize
        |    field_set %token.offset = %tmp
        |    %count = call @identity(%count) -> i32
        |    method_call %labels.push("ok") -> Unit
        |    descriptor Vec[String]::push(%labels, "ok") -> Unit
        |    %maybe = variant Option[Token]::Some(%token)
        |    %tag = variant_tag %maybe: Option[Token]
        |    %is_some = variant_is %maybe: Option[Token]::Some
        |    %payload = variant_payload %maybe.Some[0]: Token
        |    cond_branch true ? ^exit : ^error
        |  ^error:
        |    error "cosmo0.lir.test" "failed"
        |  ^exit:
        |    return %count
        |}""".stripMargin,
    )

  test("debug renderer is stable for equivalent declaration, local, and block inputs"):
    val first = LirModule(
      "core",
      List(processFunction(), identityFunction(), tokenDecl()),
    )
    val second = LirModule(
      "core",
      List(
        tokenDecl().copy(
          fields = tokenDecl().fields.reverse,
          variants = tokenDecl().variants.reverse,
        ),
        processFunction(
          locals = processLocals().reverse,
          blocks = processBlocks().reverse,
        ),
        identityFunction(),
      ),
    )

    assertEquals(
      LirDebugRenderer.renderModule(first),
      LirDebugRenderer.renderModule(second),
    )

  test("module renderer has a deterministic declaration and type-member layout"):
    val module = LirModule(
      "core",
      List(
        tokenDecl(),
        LirTypeAliasDecl(
          Lir.declId("token_id"),
          "TokenId",
          Lir.t(SourceType.Standard("Id", List(tokenType))),
        ),
        LirGlobal(
          Lir.declId("answer"),
          "answer",
          Lir.t(SourceType.I32),
          mutable = true,
          initializer = Some(Lir.int(42)),
        ),
      ),
    )

    assertEquals(
      LirDebugRenderer.renderModule(module),
      """module core {
        |  global var @answer answer: i32 = 42:i32
        |
        |  type @token Token {
        |    field var offset: usize
        |    field text: String
        |    variant Empty()
        |    variant WithText(value: String)
        |  }
        |
        |  typealias @token_id TokenId = Id[Token]
        |}""".stripMargin,
    )

  test("callable signatures retain cosmo0 source type information"):
    val span = SourceFile("<test>", "").span(0, 0)
    val sourceSignature = CallableSignature(
      "identity",
      List(CallableParam("value", SourceType.I32, span)),
      SourceType.I32,
    )
    val lirSignature = LirCallableSignature.fromSource(sourceSignature)
    val function = Lir.function(
      "identity",
      List(Lir.param("value", SourceType.I32)),
      SourceType.I32,
      locals = Nil,
      blocks = List(Lir.block("entry", terminator = LirReturn(Some(Lir.ref("value", SourceType.I32))))),
      sourceSignature = Some(sourceSignature),
    )

    assertEquals(lirSignature.params, List(Lir.t(SourceType.I32)))
    assertEquals(lirSignature.returnType, Lir.t(SourceType.I32))
    assertEquals(lirSignature.sourceSignature, Some(sourceSignature))
    assertEquals(function.signature.sourceSignature, Some(sourceSignature))
    assertEquals(function.signature.params, List(Lir.t(SourceType.I32)))

  test("reference assignability allows mutable refs as readonly values but not the reverse"):
    val boxType = SourceType.User("Box")
    val readonlyRef = SourceType.Ref(boxType, mutable = false)
    val mutableRef = SourceType.Ref(boxType, mutable = true)

    assert(SourceType.assignable(mutableRef, readonlyRef))
    assert(!SourceType.assignable(readonlyRef, mutableRef))
    assert(
      !SourceType.assignable(
        SourceType.Function(List(mutableRef), SourceType.Unit),
        SourceType.Function(List(readonlyRef), SourceType.Unit),
      ),
    )

  test("LIR type checker accepts a valid hand-written module"):
    val result = LirTypeChecker().check(checkedLirModule())

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)
    assertEquals(result.value, Some(checkedLirModule()))

  test("LIR type checker permits mutable refs for readonly arguments and rejects readonly refs for mutable arguments"):
    val boxType = SourceType.User("Box")
    val readonlyRef = SourceType.Ref(boxType, mutable = false)
    val mutableRef = SourceType.Ref(boxType, mutable = true)

    val readBox = Lir.function(
      "read_box",
      List(Lir.param("box", readonlyRef)),
      SourceType.Unit,
      locals = Nil,
      blocks = List(Lir.block("entry", terminator = LirReturn(None))),
    )
    val callRead = Lir.function(
      "call_read",
      List(Lir.param("box", mutableRef)),
      SourceType.Unit,
      locals = Nil,
      blocks = List(
        Lir.block(
          "entry",
          operations = List(
            LirDirectCall(
              None,
              Lir.declId("read_box"),
              List(Lir.ref("box", mutableRef)),
              Lir.signature(List(readonlyRef), SourceType.Unit),
            ),
          ),
          terminator = LirReturn(None),
        ),
      ),
    )
    val accepted = LirTypeChecker().check(LirModule("refs", List(readBox, callRead)))
    assertEquals(accepted.status, PhaseStatus.Succeeded)

    val mutateBox = Lir.function(
      "mutate_box",
      List(Lir.param("box", mutableRef)),
      SourceType.Unit,
      locals = Nil,
      blocks = List(Lir.block("entry", terminator = LirReturn(None))),
    )
    val callMutate = Lir.function(
      "call_mutate",
      List(Lir.param("box", readonlyRef)),
      SourceType.Unit,
      locals = Nil,
      blocks = List(
        Lir.block(
          "entry",
          operations = List(
            LirDirectCall(
              None,
              Lir.declId("mutate_box"),
              List(Lir.ref("box", readonlyRef)),
              Lir.signature(List(mutableRef), SourceType.Unit),
            ),
          ),
          terminator = LirReturn(None),
        ),
      ),
    )
    val rejected = LirTypeChecker().check(LirModule("refs", List(mutateBox, callMutate)))
    assertEquals(rejected.status, PhaseStatus.Failed)
    assert(rejected.diagnostics.exists(_.code == "cosmo0.lir.assignment-mismatch"))

  test("LIR source signatures reject mutable receiver params for readonly receivers"):
    val boxType = SourceType.User("Box")
    val readonlySource = CallableSignature(
      "inspect",
      Nil,
      SourceType.Unit,
      Some(CallableReceiver(boxType, mutable = false)),
    )
    val function = Lir.function(
      "inspect",
      List(Lir.param("self", SourceType.Ref(boxType, mutable = true))),
      SourceType.Unit,
      locals = Nil,
      blocks = List(Lir.block("entry", terminator = LirReturn(None))),
      sourceSignature = Some(readonlySource),
    )

    val result = LirTypeChecker().check(LirModule("readonly", List(function)))

    assertEquals(result.status, PhaseStatus.Failed)
    assert(result.diagnostics.exists(_.code == "cosmo0.lir.invalid-signature"))

  test("LIR type checker rejects structural, type, call, variant, and mutability errors"):
    val cases = List(
      missingBlockModule() -> "cosmo0.lir.missing-block",
      invalidBranchModule() -> "cosmo0.lir.invalid-branch",
      invalidBranchConditionModule() -> "cosmo0.lir.invalid-branch",
      invalidReturnModule() -> "cosmo0.lir.invalid-return",
      typeMismatchModule() -> "cosmo0.lir.assignment-mismatch",
      useBeforeDefinitionModule() -> "cosmo0.lir.use-before-definition",
      invalidCallModule() -> "cosmo0.lir.invalid-call",
      invalidDescriptorModule() -> "cosmo0.lir.invalid-descriptor",
      invalidVariantModule() -> "cosmo0.lir.invalid-variant",
      invalidMutabilityModule() -> "cosmo0.lir.invalid-mutability",
    )

    cases.foreach { case (module, code) =>
      val result = LirTypeChecker().check(module)

      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, PhaseStatus.Failed)
      assert(
        result.diagnostics.exists(_.code == code),
        s"missing diagnostic $code in ${result.diagnostics.map(_.code)}",
      )
    }

  test("LIR type checker rejects ordinary runtime APIs as descriptor families"):
    val cases = List(
      LirDescriptorRef("Runtime") -> "println",
      LirDescriptorRef("Json") -> "parse",
      LirDescriptorRef("Filesystem") -> "read_file",
      LirDescriptorRef("Command") -> "run",
      LirDescriptorRef("StringBuilder") -> "<init>",
      LirDescriptorRef("TextBuilder") -> "<init>",
      LirDescriptorRef("TextView") -> "slice",
      LirDescriptorRef("SourceText") -> "len",
    )

    cases.foreach { case (descriptor, operation) =>
      val result = LirTypeChecker().check(rejectedDescriptorModule(descriptor, operation))

      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, PhaseStatus.Failed)
      assert(
        result.diagnostics.exists(_.code == "cosmo0.lir.invalid-descriptor"),
        s"missing invalid descriptor diagnostic for ${descriptor.name}::$operation in ${result.diagnostics.map(_.code)}",
      )
    }

    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("StringBuilder"))
    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("Json"))
    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("JsonValue"))
    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("TextBuilder"))
    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("TextView"))
    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("SourceText"))
    assert(StandardGenericDescriptors.get("Json").isEmpty)
    assert(StandardGenericDescriptors.get("JsonValue").isEmpty)
    assert(StandardGenericDescriptors.get("StringBuilder").isEmpty)
    assert(StandardGenericDescriptors.get("TextBuilder").isEmpty)
    assert(StandardGenericDescriptors.get("TextView").isEmpty)
    assert(StandardGenericDescriptors.get("SourceText").isEmpty)

  test("extern C++ symbols are structured qualified names"):
    val symbol = CppQualifiedSymbol.global("cosmo0_runtime", "println")

    assertEquals(symbol.namespace, List("cosmo0_runtime"))
    assertEquals(symbol.name, "println")
    assertEquals(symbol.canonical, "cosmo0_runtime::println")
    assertEquals(symbol.cppName, "::cosmo0_runtime::println")
    assertEquals(BackendRequirement.runtimeSymbol(symbol).legacyName, "runtime-symbol:cosmo0_runtime::println")
    intercept[IllegalArgumentException] {
      CppQualifiedSymbol.parse("cosmo0_runtime::println(value)")
    }

  test("LIR type checker diagnostics are deterministic and identify failing constructs"):
    val first = invalidBranchModule()
    val second = first.copy(declarations = first.declarations.reverse)

    val firstDiagnostics = LirTypeChecker().check(first).diagnostics.map(d => d.code -> d.message)
    val secondDiagnostics = LirTypeChecker().check(second).diagnostics.map(d => d.code -> d.message)

    assertEquals(firstDiagnostics, secondDiagnostics)
    assert(
      firstDiagnostics.exists { case (_, message) => message.contains("^missing") },
      s"expected branch target in diagnostics: $firstDiagnostics",
    )

  test("LIR node classes live in the cosmo0 package boundary"):
    val modelClasses = List(
      classOf[LirModule],
      classOf[LirFunction],
      classOf[LirBlock],
      classOf[LirOp],
      classOf[LirValue],
      classOf[LirTerminator],
    )

    assert(modelClasses.forall(_.getName.startsWith("cosmo0.")))

  private def checkedLirModule(function: LirFunction = checkedProcessFunction()): LirModule =
    LirModule(
      "checked",
      List(tokenDecl(), identityFunction(), function),
    )

  private def checkedProcessFunction(
      entryOperations: List[LirOp] = checkedEntryOperations(),
      entryTerminator: LirTerminator = LirCondBranch(
        Lir.bool(true),
        Lir.label("exit"),
        Lir.label("error"),
      ),
      returnTerminator: LirTerminator = LirReturn(Some(Lir.ref("count", SourceType.I32))),
  ): LirFunction =
    Lir.function(
      "checked_process",
      List(Lir.param("input", tokenType)),
      SourceType.I32,
      locals = checkedLocals(),
      blocks = List(
        Lir.block(
          "entry",
          operations = entryOperations,
          terminator = entryTerminator,
        ),
        Lir.block(
          "exit",
          terminator = returnTerminator,
        ),
        Lir.block(
          "error",
          terminator = LirErrorExit("cosmo0.lir.test", Some("failed")),
        ),
      ),
    )

  private def checkedLocals(): List[LirLocal] =
    List(
      Lir.local("token", tokenType, mutable = true),
      Lir.local("labels", stringVecType, mutable = true),
      Lir.local("maybe", optionTokenType),
      Lir.local("is_some", SourceType.Bool),
      Lir.local("tag", SourceType.I32),
      Lir.local("payload", tokenType),
      Lir.local("tmp", SourceType.Usize),
      Lir.local("count", SourceType.I32, mutable = true),
    )

  private def checkedEntryOperations(): List[LirOp] =
    List(
      LirAllocLocal(Lir.local("token", tokenType, mutable = true)),
      LirAllocLocal(Lir.local("labels", stringVecType, mutable = true)),
      LirAllocLocal(Lir.local("count", SourceType.I32, mutable = true), Some(Lir.int(0))),
      LirFieldGet(
        Lir.localId("tmp"),
        Lir.ref("input", tokenType),
        "offset",
        Lir.t(SourceType.Usize),
      ),
      LirFieldSet(
        Lir.ref("token", tokenType),
        "offset",
        Lir.ref("tmp", SourceType.Usize),
      ),
      LirAssign(Lir.localPlace("count", SourceType.I32), Lir.int(1)),
      LirDirectCall(
        Some(Lir.localId("count")),
        Lir.declId("identity"),
        List(Lir.ref("count", SourceType.I32)),
        Lir.signature(List(SourceType.I32), SourceType.I32),
      ),
      LirLoweredMethodCall(
        None,
        Lir.ref("labels", stringVecType),
        "push",
        List(Lir.string("ok")),
        Lir.signature(List(SourceType.String), SourceType.Unit),
      ),
      LirDescriptorIntrinsic(
        None,
        LirDescriptorRef("Vec", List(Lir.t(SourceType.String))),
        "push",
        List(Lir.ref("labels", stringVecType), Lir.string("ok")),
        Some(Lir.t(SourceType.Unit)),
      ),
      LirConstructVariant(
        Lir.localId("maybe"),
        Lir.t(optionTokenType),
        "Some",
        List(Lir.ref("token", tokenType)),
      ),
      LirReadVariantTag(
        Lir.localId("tag"),
        Lir.ref("maybe", optionTokenType),
        Lir.t(optionTokenType),
      ),
      LirCheckVariantTag(
        Lir.localId("is_some"),
        Lir.ref("maybe", optionTokenType),
        Lir.t(optionTokenType),
        "Some",
      ),
      LirReadVariantPayload(
        Lir.localId("payload"),
        Lir.ref("maybe", optionTokenType),
        "Some",
        0,
        Lir.t(tokenType),
      ),
    )

  private def missingBlockModule(): LirModule =
    checkedLirModule(
      Lir.function(
        "checked_process",
        List(Lir.param("input", tokenType)),
        SourceType.I32,
        locals = checkedLocals(),
        blocks = Nil,
      ),
    )

  private def invalidBranchModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryTerminator = LirCondBranch(
          Lir.bool(true),
          Lir.label("exit"),
          Lir.label("missing"),
        ),
      ),
    )

  private def invalidBranchConditionModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryTerminator = LirCondBranch(
          Lir.ref("count", SourceType.I32),
          Lir.label("exit"),
          Lir.label("error"),
        ),
      ),
    )

  private def invalidReturnModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        returnTerminator = LirReturn(Some(Lir.bool(false))),
      ),
    )

  private def typeMismatchModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryOperations =
          checkedEntryOperations() :+ LirAssign(Lir.localPlace("count", SourceType.I32), Lir.bool(false)),
      ),
    )

  private def useBeforeDefinitionModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryOperations =
          LirAssign(Lir.localPlace("count", SourceType.I32), Lir.ref("tmp", SourceType.Usize)) :: checkedEntryOperations(),
      ),
    )

  private def invalidCallModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryOperations = checkedEntryOperations().map {
          case LirDirectCall(output, callee, args, _) =>
            LirDirectCall(output, callee, args, Lir.signature(List(SourceType.Bool), SourceType.I32))
          case other => other
        },
      ),
    )

  private def invalidDescriptorModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryOperations = checkedEntryOperations().map {
          case LirDescriptorIntrinsic(output, descriptor, name, args, _) =>
            LirDescriptorIntrinsic(output, descriptor, name, args, Some(Lir.t(SourceType.I32)))
          case other => other
        },
      ),
    )

  private def rejectedDescriptorModule(
      descriptor: LirDescriptorRef,
      operation: String,
  ): LirModule =
    LirModule(
      s"rejected_${descriptor.name.toLowerCase}",
      List(
        Lir.function(
          "bad",
          Nil,
          SourceType.Unit,
          locals = Nil,
          blocks = List(
            Lir.block(
              "entry",
              operations = List(
                LirDescriptorIntrinsic(
                  None,
                  descriptor,
                  operation,
                  Nil,
                  Some(Lir.t(SourceType.Unit)),
                ),
              ),
              terminator = LirReturn(None),
            ),
          ),
        ),
      ),
    )

  private def invalidVariantModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryOperations = checkedEntryOperations().map {
          case LirConstructVariant(output, owner, _, payload) =>
            LirConstructVariant(output, owner, "Missing", payload)
          case other => other
        },
      ),
    )

  private def invalidMutabilityModule(): LirModule =
    checkedLirModule(
      checkedProcessFunction(
        entryOperations = checkedEntryOperations().map {
          case LirFieldSet(_, field, value) =>
            LirFieldSet(Lir.ref("input", tokenType), field, value)
          case other => other
        },
      ),
    )

  private def tokenDecl(): LirTypeDecl =
    LirTypeDecl(
      Lir.declId("token"),
      "Token",
      fields = List(
        LirField("text", Lir.t(SourceType.String)),
        LirField("offset", Lir.t(SourceType.Usize), mutable = true),
      ),
      variants = List(
        LirVariant("WithText", List(LirVariantPayload(Some("value"), Lir.t(SourceType.String)))),
        LirVariant("Empty"),
      ),
    )

  private def identityFunction(): LirFunction =
    Lir.function(
      "identity",
      List(Lir.param("value", SourceType.I32)),
      SourceType.I32,
      locals = Nil,
      blocks = List(
        Lir.block(
          "entry",
          terminator = LirReturn(Some(Lir.ref("value", SourceType.I32))),
        ),
      ),
    )

  private def processFunction(
      locals: List[LirLocal] = processLocals(),
      blocks: List[LirBlock] = processBlocks(),
  ): LirFunction =
    Lir.function(
      "process",
      List(Lir.param("input", tokenType)),
      SourceType.I32,
      locals = locals,
      blocks = blocks,
    )

  private def processLocals(): List[LirLocal] =
    List(
      Lir.local("token", tokenType, mutable = true),
      Lir.local("labels", stringVecType, mutable = true),
      Lir.local("maybe", optionTokenType),
      Lir.local("is_some", SourceType.Bool),
      Lir.local("tag", SourceType.I32),
      Lir.local("payload", tokenType),
      Lir.local("tmp", SourceType.Usize),
      Lir.local("count", SourceType.I32, mutable = true),
    )

  private def processBlocks(): List[LirBlock] =
    List(
      Lir.block(
        "exit",
        terminator = LirReturn(Some(Lir.ref("count", SourceType.I32))),
      ),
      Lir.block(
        "entry",
        operations = List(
          LirAllocLocal(Lir.local("count", SourceType.I32, mutable = true), Some(Lir.int(0))),
          LirAssign(Lir.localPlace("count", SourceType.I32), Lir.int(1)),
          LirFieldGet(
            Lir.localId("tmp"),
            Lir.ref("token", tokenType),
            "offset",
            Lir.t(SourceType.Usize),
          ),
          LirFieldSet(
            Lir.ref("token", tokenType),
            "offset",
            Lir.ref("tmp", SourceType.Usize),
          ),
          LirDirectCall(
            Some(Lir.localId("count")),
            Lir.declId("identity"),
            List(Lir.ref("count", SourceType.I32)),
            Lir.signature(List(SourceType.I32), SourceType.I32),
          ),
          LirLoweredMethodCall(
            None,
            Lir.ref("labels", stringVecType),
            "push",
            List(Lir.string("ok")),
            Lir.signature(List(SourceType.String), SourceType.Unit),
          ),
          LirDescriptorIntrinsic(
            None,
            LirDescriptorRef("Vec", List(Lir.t(SourceType.String))),
            "push",
            List(Lir.ref("labels", stringVecType), Lir.string("ok")),
            Some(Lir.t(SourceType.Unit)),
          ),
          LirConstructVariant(
            Lir.localId("maybe"),
            Lir.t(optionTokenType),
            "Some",
            List(Lir.ref("token", tokenType)),
          ),
          LirReadVariantTag(
            Lir.localId("tag"),
            Lir.ref("maybe", optionTokenType),
            Lir.t(optionTokenType),
          ),
          LirCheckVariantTag(
            Lir.localId("is_some"),
            Lir.ref("maybe", optionTokenType),
            Lir.t(optionTokenType),
            "Some",
          ),
          LirReadVariantPayload(
            Lir.localId("payload"),
            Lir.ref("maybe", optionTokenType),
            "Some",
            0,
            Lir.t(tokenType),
          ),
        ),
        terminator = LirCondBranch(
          Lir.bool(true),
          Lir.label("exit"),
          Lir.label("error"),
        ),
      ),
      Lir.block(
        "dispatch",
        terminator = LirBranch(Lir.label("entry")),
      ),
      Lir.block(
        "dead",
        terminator = LirUnreachable(Some("not scheduled")),
      ),
      Lir.block(
        "error",
        terminator = LirErrorExit("cosmo0.lir.test", Some("failed")),
      ),
    )
