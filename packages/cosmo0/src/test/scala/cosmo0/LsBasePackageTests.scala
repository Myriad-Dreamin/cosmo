package cosmo0

class LsBasePackageTests extends munit.FunSuite:
  test("ls-base JSON-RPC core package compiles as a runnable package"):
    val path = "packages/ls-base"
    val loaded = Cosmo0().loadPackage(path)

    assertEquals(loaded.phase, Phase.Check)
    assert(
      loaded.isSuccess,
      s"ls-base package load failed with diagnostics: ${loaded.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(loaded.value.get.metadata.stageProfile, Some(StageCapabilityRegistry.Cosmo1Stage1))
    assertEquals(
      loaded.value.get.modules.map(_.modulePath),
      List(
        List("jsonrpc", "core"),
        List("jsonrpc", "core_test"),
        List("jsonrpc", "session"),
        List("lsp", "lifecycle"),
        List("lsp", "lifecycle_test"),
        List("main"),
        List("std", "bytes"),
        List("std", "char_class"),
        List("std", "char_class_test"),
        List("std", "json"),
        List("std", "json_test"),
        List("std", "map_set"),
        List("std", "map_set_test"),
        List("std", "path_fs"),
        List("std", "text"),
        List("std", "text_output"),
      ),
    )

    val compiled = Cosmo0().compileRunnablePackage(path)

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"ls-base package compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assertEquals(output.moduleName, "cosmo_ls_base")
    assert(output.source.contains("inline std::string jsonrpc_encode_request("))
    assert(output.source.contains("inline cosmo0_runtime::Result<JsonRpcMessage, JsonRpcDecodeError> jsonrpc_decode_message("))
    assert(output.source.contains("inline JsonRpcSession jsonrpc_session()"))
    assert(output.source.contains("inline bool jsonrpc_test_session_correlation()"))
    assert(output.source.contains("inline LspServer lsp_server()"))
    assert(output.source.contains("respond_success("))
    assert(output.source.contains("inline bool lsp_test_lifecycle_transitions()"))
    assert(output.source.contains("inline bool lsp_test_hover_response_and_capability()"))
