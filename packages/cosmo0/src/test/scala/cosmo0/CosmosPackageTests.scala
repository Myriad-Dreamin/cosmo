package cosmo0

class CosmosPackageTests extends munit.FunSuite:
  test("cosmos package loads workspace, document, session, URI, and LSP bridge modules"):
    val loaded = Cosmo0().loadPackage("packages/cosmos")

    assertEquals(loaded.phase, Phase.Check)
    assert(
      loaded.isSuccess,
      s"cosmos package load failed with diagnostics: ${loaded.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(loaded.value.get.metadata.name, "@cosmo/cosmos")
    assertEquals(loaded.value.get.metadata.stageProfile, Some(StageCapabilityRegistry.Cosmo1Stage1))
    assert(
      loaded.value.get.modules.exists(_.modulePath == List("workspace", "workspace")),
      s"workspace module missing from ${loaded.value.get.modules.map(_.modulePath)}",
    )
    assert(
      loaded.value.get.modules.exists(_.modulePath == List("lsp", "document_events")),
      s"LSP document event bridge missing from ${loaded.value.get.modules.map(_.modulePath)}",
    )

  test("cosmos package checks without diagnostics and exposes focused workspace APIs"):
    val checked = Cosmo0().checkPackage("packages/cosmos")

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"cosmos package check failed with diagnostics: ${checked.diagnostics.map(d => d.code -> d.message)}",
    )

    val declarations = checked.value.get.checked.typed.declarations.map(_.name).toSet
    List(
      "CosmosWorkspace",
      "CosmosTextDocumentSnapshot",
      "CosmosOpenDocuments",
      "CosmosPackageSession",
      "CosmosDocumentLocation",
      "CosmosLspDocumentEvent",
      "CosmosDiagnosticsState",
      "CosmosPackageModuleInput",
      "CosmosLspDiagnostic",
      "CosmosLspRange",
      "CosmosLspPosition",
      "CosmosHover",
      "cosmos_normalize_document_uri",
      "cosmos_workspace",
      "cosmos_open_documents",
      "cosmos_lsp_document_event",
      "cosmos_analyze_document",
      "cosmos_analyze_package_graph",
      "cosmos_lsp_diagnostics_json",
      "cosmos_hover",
      "cosmos_hover_result_json",
      "cosmos_enable_hover",
    ).foreach { name =>
      assert(declarations.contains(name), s"missing Cosmos declaration $name")
    }

    assert(checked.value.get.moduleOrder.contains("uri_sys"))
    assert(checked.value.get.moduleOrder.contains("lsp/lifecycle"))
    assert(checked.value.get.moduleOrder.contains("lsp/diagnostics"))
    assert(checked.value.get.moduleOrder.contains("lsp/hover"))
    assert(checked.value.get.moduleOrder.last == "workspace/workspace_test")

  test("cosmos package compiles without diagnostics"):
    val compiled = Cosmo0().compilePackage("packages/cosmos")

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmos package compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )

    val output = compiled.value.get.output
    assertEquals(output.moduleName, "cosmo_cosmos")
    assert(output.source.contains("struct CosmosWorkspace"))
    assert(output.source.contains("struct CosmosTextDocumentSnapshot"))
    assert(output.source.contains("struct CosmosDiagnosticsState"))
    assert(output.source.contains("struct CosmosLspDiagnostic"))
    assert(output.source.contains("struct CosmosHover"))
    assert(output.source.contains("inline CosmosWorkspace cosmos_workspace()"))
    assert(output.source.contains("inline std::vector<CosmosLspDiagnostic> cosmos_analyze_document("))
    assert(output.source.contains("cosmos_hover("))
    assert(output.source.contains("inline bool cosmos_test_parser_diagnostic_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_checker_diagnostic_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_helloworld_sample_has_no_diagnostics()"))
    assert(output.source.contains("inline bool cosmos_test_helloworld_invalid_edit_reports_parser_diagnostic()"))
    assert(output.source.contains("inline bool cosmos_test_checker_gap_cascades_are_suppressed()"))
    assert(output.source.contains("inline bool cosmos_test_package_diagnostic_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_publish_and_clear_diagnostics()"))
    assert(output.source.contains("inline bool cosmos_test_publish_refresh_keeps_other_document()"))
    assert(output.source.contains("inline bool cosmos_test_unchanged_refresh_is_stable()"))
    assert(output.source.contains("inline bool cosmos_test_diagnostics_normalize_uri()"))
    assert(output.source.contains("inline bool cosmos_test_hover_locals_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_hover_functions_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_hover_members_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_hover_types_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_hover_unsupported_positions_are_empty()"))
    assert(output.source.contains("inline bool cosmos_test_hover_server_response_fixture()"))
    assert(output.source.contains("inline bool cosmos_test_workspace_selects_module_path()"))
    assert(output.source.contains("inline bool cosmos_test_document_snapshots_change_and_close()"))
    assert(output.source.contains("inline bool cosmos_test_package_session_cache_reuses_session()"))
    assert(output.source.contains("inline bool cosmos_test_lsp_event_bridge_keeps_uri()"))
