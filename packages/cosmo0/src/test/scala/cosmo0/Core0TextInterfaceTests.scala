package cosmo0

class Core0TextInterfaceTests extends munit.FunSuite:
  private val core0TextPath = "packages/cosmoc/src/core0/text.cos"
  private val sourceTextPath = "packages/cosmoc/src/source/source.cos"
  private val sourceMapPath = "packages/cosmoc/src/source/source_map.cos"
  private val sourceFixturePath = "fixtures/cosmo0/cosmo1/source-text/source_text_fixture.cos"
  private val sourceTestPath = "packages/cosmoc/src/source/source_test.cos"
  private val sourceMapTestPath = "packages/cosmoc/src/source/source_map_test.cos"

  test("direct core0.text source APIs cover length, slicing, char and byte access, and builders"):
    val source = combineSources(
      List(core0TextPath),
      """def core0_text_direct_smoke(): Bool = {
        |  val text = "abcdef";
        |  val view = core0_text_view(text, 1, 4);
        |  var builder = core0_text_builder();
        |  builder.push("view=");
        |  builder.push_view(view);
        |  builder.push_line("");
        |  val rendered = builder.finish();
        |
        |  core0_text_len(text) == 6 and
        |    !core0_text_is_empty(text) and
        |    core0_text_is_empty("") and
        |    core0_text_slice(text, 2, 5) == "cde" and
        |    core0_text_byte_at(text, 1) == 98 and
        |    core0_text_char_at(text, 0) != core0_text_char_at(text, 1) and
        |    view.len() == 3 and
        |    !view.is_empty() and
        |    view.to_string() == "bcd" and
        |    view.byte_at(0) == 98 and
        |    view.char_at(0) != view.char_at(1) and
        |    rendered == "view=bcd\n"
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("core0_text_direct.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.text direct lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @core0_text_len core0_text_len"))
    assert(rendered.contains("fn @TextBuilder.push push"))
    assert(rendered.contains("fn @TextView.byte_at byte_at"))
    assert(rendered.contains("descriptor String::slice("))
    assert(rendered.contains("descriptor String::char_at("))
    assert(rendered.contains("descriptor String::byte_at("))
    assert(!rendered.contains("descriptor TextBuilder"))
    assert(!rendered.contains("descriptor TextView"))
    assert(!rendered.contains("descriptor StringBuilder"))

    val compiled = CppBackend().emit(lowered.value.get.lir)

    assert(
      compiled.isSuccess,
      s"core0.text direct C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.source
    assert(output.contains("struct TextBuilder"))
    assert(output.contains("struct TextView"))
    assert(output.contains("inline std::size_t core0_text_len(std::string text)"))
    assert(output.contains(".substr("))
    assert(output.contains(".at("))
    assert(!output.contains("StringBuilder"))

  test("cosmo1 source text fixture uses SourceText and SourceMap helpers"):
    val source = combineSources(
      List(core0TextPath, sourceTextPath, sourceMapPath, sourceFixturePath),
      "",
    )

    val result = Cosmo0().compile(SourceFile(sourceFixturePath, source))

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"cosmo1 source text fixture compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val output = result.value.get.output
    assert(output.contains("struct SourceText"))
    assert(output.contains("struct SourceMap"))
    assert(output.contains("struct SourceLocation"))
    assert(output.contains("inline bool source_text_fixture()"))
    assert(output.contains("inline std::string source_text_slice("))
    assert(output.contains("inline SourceLocation SourceMap_location("))
    assert(!output.contains("StringBuilder"))

  test("source.source helpers expose SourceText operations without descriptor families"):
    val source = combineSources(
      List(core0TextPath, sourceTextPath, sourceTestPath),
      "",
    )

    val lowered = Cosmo0().lower(SourceFile(sourceTestPath, source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"source.source helper lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @source_text_from_string source_text_from_string"))
    assert(rendered.contains("fn @source_text_len source_text_len"))
    assert(rendered.contains("fn @source_text_slice source_text_slice"))
    assert(rendered.contains("fn @source_text_view source_text_view"))
    assert(rendered.contains("method_call %source.len() -> usize"))
    assert(rendered.contains("method_call %view.to_string() -> String"))
    assert(!rendered.contains("descriptor SourceText"))
    assert(!rendered.contains("descriptor TextView"))

    val compiled = CppBackend().emit(lowered.value.get.lir)

    assert(
      compiled.isSuccess,
      s"source.source helper C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.source
    assert(output.contains("inline SourceText source_text_from_string(std::string text)"))
    assert(output.contains("inline std::size_t source_text_len(SourceText source)"))
    assert(output.contains("inline std::string source_text_slice(SourceText source"))
    assert(output.contains("inline TextView source_text_view(SourceText source"))
    assert(!output.contains("SourceText_descriptor"))

  test("source.source_map helpers expose line and location APIs without descriptor families"):
    val source = combineSources(
      List(core0TextPath, sourceTextPath, sourceMapPath, sourceMapTestPath),
      "",
    )

    val lowered = Cosmo0().lower(SourceFile(sourceMapTestPath, source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"source.source_map helper lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @make_source_map make_source_map"))
    assert(rendered.contains("fn @source_map_clamp_offset source_map_clamp_offset"))
    assert(rendered.contains("method_call %map.location(3:usize) -> SourceLocation"))
    assert(rendered.contains("method_call %map.line_start(3:usize) -> usize"))
    assert(rendered.contains("method_call %map.line_end(3:usize) -> usize"))
    assert(rendered.contains("method_call %map.line_text(5:usize) -> String"))
    assert(!rendered.contains("descriptor SourceMap"))
    assert(!rendered.contains("descriptor SourceLocation"))

    val compiled = CppBackend().emit(lowered.value.get.lir)

    assert(
      compiled.isSuccess,
      s"source.source_map helper C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.source
    assert(output.contains("struct SourceMap"))
    assert(output.contains("struct SourceLocation"))
    assert(output.contains("inline SourceMap make_source_map(SourceText source)"))
    assert(output.contains("inline SourceLocation SourceMap_location("))
    assert(output.contains("inline std::size_t SourceMap_line_start("))
    assert(output.contains("inline std::string SourceMap_line_text("))
    assert(!output.contains("SourceMap_descriptor"))

  private def combineSources(paths: List[String], extra: String): String =
    (paths.map(ParserFixtureManifest.readFile) :+ extra).mkString("\n")
