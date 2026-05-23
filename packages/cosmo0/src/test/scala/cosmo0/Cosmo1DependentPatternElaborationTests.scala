package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

class Cosmo1DependentPatternElaborationTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val profilePath = "packages/cosmoc/src/types/profile.cos"
  private val dependentPatternPath = "packages/cosmoc/src/types/dependent_pattern.cos"
  private val dependentPatternTestPath = "packages/cosmoc/src/types/dependent_pattern_test.cos"

  test("cosmo1 dependent pattern elaboration source compiles"):
    val source = combineSources(
      List(
        spanPath,
        profilePath,
        dependentPatternPath,
        dependentPatternTestPath,
      ),
    )

    val compiled = Cosmo0().compile(SourceFile(dependentPatternTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 dependent pattern compile failed with diagnostics: ${compiled.diagnostics.map(d => (d.code, d.message, d.span.map(span => span.start -> span.end)))}",
    )
    val output = compiled.value.get.output
    assert(output.contains("struct DependentPatternConstructorDecl"))
    assert(output.contains("struct DependentPatternUnifier"))
    assert(output.contains("struct DependentCaseTree"))
    assert(output.contains("inline CheckerProfile checker_profile_mltt_dependent_patterns()"))
    assert(output.contains("inline DependentCaseTree elaborate_dependent_pattern_clauses("))
    assert(output.contains("inline bool dependent_pattern_profile_metadata_declares_elaboration_support()"))
    assert(output.contains("inline bool dependent_pattern_unifier_detects_impossible_constructor_index()"))
    assert(output.contains("inline bool dependent_pattern_vec_head_elaborates_cons_and_omits_impossible_nil()"))
    assert(output.contains("inline bool dependent_pattern_vec_append_fixture_elaborates_constructor_split()"))
    assert(output.contains("inline bool dependent_pattern_variable_wildcard_and_impossible_patterns_elaborate()"))
    assert(output.contains("inline bool dependent_pattern_unsupported_profile_rejects_clauses()"))
    assert(output.contains("inline bool dependent_pattern_equality_pattern_is_rejected()"))
    assert(output.contains("inline bool dependent_pattern_coverage_reports_missing_branch()"))
    assert(output.contains("inline bool dependent_pattern_coverage_reports_redundant_branch_with_span()"))
    assert(output.contains("cosmo.type.unsupported-dependent-pattern"))
    assert(output.contains("cosmo.type.unsupported-equality-pattern"))
    assert(output.contains("cosmo.type.dependent-pattern.missing-branch"))
    assert(output.contains("cosmo.type.dependent-pattern.redundant-branch"))
    assert(output.contains("int main()"))

  test("cosmo1 dependent pattern fixture executable passes"):
    val compiled = compileDependentPatternTestProgram()

    assert(
      compiled.isSuccess,
      s"cosmo1 dependent pattern compile failed with diagnostics: ${compiled.diagnostics.map(d => (d.code, d.message, d.span.map(span => span.start -> span.end)))}",
    )

    val compiler = cxxCompiler().getOrElse(fail("no C++ compiler found for dependent pattern fixture execution test"))
    DependentPatternNodeFs.mkdirSync("target/cosmo1-dependent-pattern-tests", js.Dynamic.literal("recursive" -> true))
    val sourcePath = "target/cosmo1-dependent-pattern-tests/dependent_pattern_test.cpp"
    val executablePath = "target/cosmo1-dependent-pattern-tests/dependent_pattern_test"
    DependentPatternNodeFs.writeFileSync(sourcePath, compiled.value.get.output)

    val compile = DependentPatternNodeSpawnSync(
      compiler,
      js.Array("-std=c++17", NlohmannJsonDependency.includeArg, sourcePath, "-o", executablePath),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      compile.status.toOption,
      Some(0),
      s"C++ compiler rejected dependent pattern fixture output with ${compiler}\n${compile.stderr.getOrElse("")}",
    )

    val run = DependentPatternNodeSpawnSync(
      executablePath,
      js.Array(),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      run.status.toOption,
      Some(0),
      s"dependent pattern fixture executable failed\nstdout:\n${run.stdout.getOrElse("")}\nstderr:\n${run.stderr.getOrElse("")}",
    )

  private def compileDependentPatternTestProgram(): Result[CompiledModule] =
    val source = combineSources(
      List(
        spanPath,
        profilePath,
        dependentPatternPath,
        dependentPatternTestPath,
      ),
    )
    Cosmo0().compile(SourceFile(dependentPatternTestPath, source))

  private def cxxCompiler(): Option[String] =
    List("c++", "g++", "clang++").find { command =>
      val result = DependentPatternNodeSpawnSync(
        command,
        js.Array("--version"),
        js.Dynamic.literal(encoding = "utf8"),
      )
      result.status.toOption.contains(0)
    }

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1DependentPatternElaborationTests

@js.native
@JSImport("node:child_process", "spawnSync")
private object DependentPatternNodeSpawnSync extends js.Object:
  def apply(command: String, args: js.Array[String], options: js.Any): DependentPatternNodeSpawnSyncResult = js.native

@js.native
private trait DependentPatternNodeSpawnSyncResult extends js.Object:
  val status: js.UndefOr[Int] = js.native
  val stdout: js.UndefOr[String] = js.native
  val stderr: js.UndefOr[String] = js.native

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object DependentPatternNodeFs extends js.Object:
  def mkdirSync(path: String, options: js.Any): Unit = js.native
  def writeFileSync(path: String, data: String): Unit = js.native
