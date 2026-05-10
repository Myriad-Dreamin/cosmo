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
