package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

class Cosmo1MlttTypeCheckerTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val profilePath = "packages/cosmoc/src/types/profile.cos"
  private val corePath = "packages/cosmoc/src/types/mltt/core.cos"
  private val checkPath = "packages/cosmoc/src/types/mltt/check.cos"
  private val checkTestPath = "packages/cosmoc/src/types/mltt/check_test.cos"

  test("cosmo1 MLTT checker source compiles"):
    val source = combineSources(
      List(
        spanPath,
        profilePath,
        corePath,
        checkPath,
        checkTestPath,
      ),
    )

    val compiled = Cosmo0().compile(SourceFile(checkTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 MLTT checker compile failed with diagnostics: ${compiled.diagnostics.map(d => (d.code, d.message, d.span.map(span => span.start -> span.end)))}",
    )
    val output = compiled.value.get.output
    assert(output.contains("struct MlttTerm"))
    assert(output.contains("struct MlttContextEntry"))
    assert(output.contains("struct MlttInductiveDecl"))
    assert(output.contains("struct MlttConversionResult"))
    assert(output.contains("inline MlttCheckResult mltt_check("))
    assert(output.contains("inline MlttInferResult mltt_infer("))
    assert(output.contains("inline MlttConversionResult mltt_convert_with_strategy("))
    assert(output.contains("inline bool mltt_lambda_checks_against_pi()"))
    assert(output.contains("inline bool mltt_conversion_accepts_beta_and_let()"))
    assert(output.contains("inline bool mltt_conversion_reduces_transparent_nat_definition()"))
    assert(output.contains("inline bool mltt_conversion_rejects_effectful_definition()"))
    assert(output.contains("inline bool mltt_vec_constructor_signatures_check_without_pattern_matching()"))
    assert(output.contains("inline bool mltt_nat_and_vec_declaration_metadata_is_deterministic()"))
    assert(output.contains("cosmo.type.mltt.unsupported-normalization-profile"))
    assert(output.contains("cosmo.type.mltt.effectful-conversion"))
    assert(output.contains("mltt.whnf-conversion"))
    assert(output.contains("int main()"))

  test("cosmo1 MLTT checker fixture executable passes"):
    val compiled = compileMlttTestProgram()

    assert(
      compiled.isSuccess,
      s"cosmo1 MLTT checker compile failed with diagnostics: ${compiled.diagnostics.map(d => (d.code, d.message, d.span.map(span => span.start -> span.end)))}",
    )

    val compiler = cxxCompiler().getOrElse(fail("no C++ compiler found for MLTT fixture execution test"))
    MlttNodeFs.mkdirSync("target/cosmo1-mltt-tests", js.Dynamic.literal("recursive" -> true))
    val sourcePath = "target/cosmo1-mltt-tests/mltt_check_test.cpp"
    val executablePath = "target/cosmo1-mltt-tests/mltt_check_test"
    MlttNodeFs.writeFileSync(sourcePath, compiled.value.get.output)

    val compile = MlttNodeSpawnSync(
      compiler,
      js.Array("-std=c++17", NlohmannJsonDependency.includeArg, sourcePath, "-o", executablePath),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      compile.status.toOption,
      Some(0),
      s"C++ compiler rejected MLTT fixture output with $compiler\n${compile.stderr.getOrElse("")}",
    )

    val run = MlttNodeSpawnSync(
      executablePath,
      js.Array(),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      run.status.toOption,
      Some(0),
      s"MLTT fixture executable failed\nstdout:\n${run.stdout.getOrElse("")}\nstderr:\n${run.stderr.getOrElse("")}",
    )

  private def compileMlttTestProgram(): Result[CompiledModule] =
    val source = combineSources(
      List(
        spanPath,
        profilePath,
        corePath,
        checkPath,
        checkTestPath,
      ),
    )
    Cosmo0().compile(SourceFile(checkTestPath, source))

  private def cxxCompiler(): Option[String] =
    List("c++", "g++", "clang++").find { command =>
      val result = MlttNodeSpawnSync(
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
end Cosmo1MlttTypeCheckerTests

@js.native
@JSImport("node:child_process", "spawnSync")
private object MlttNodeSpawnSync extends js.Object:
  def apply(command: String, args: js.Array[String], options: js.Any): MlttNodeSpawnSyncResult = js.native

@js.native
private trait MlttNodeSpawnSyncResult extends js.Object:
  val status: js.UndefOr[Int] = js.native
  val stdout: js.UndefOr[String] = js.native
  val stderr: js.UndefOr[String] = js.native

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object MlttNodeFs extends js.Object:
  def mkdirSync(path: String, options: js.Any): Unit = js.native
  def writeFileSync(path: String, data: String): Unit = js.native
