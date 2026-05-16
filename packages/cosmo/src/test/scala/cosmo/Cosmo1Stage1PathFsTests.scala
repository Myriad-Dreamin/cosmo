package cosmo

class Cosmo1Stage1PathFsTest extends TestBase:
  private val sourceFiles = List(
    "library/std/src/std/path_fs.cos",
    "packages/cosmoc/src/driver/config.cos",
    "packages/cosmoc/src/source/source.cos",
    "packages/cosmoc/src/source/source_test.cos",
  )

  test("cosmoc Stage 1 path fs source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse Stage 1 path fs source: $path")
    }
  }

  test("cosmoc manifest includes path fs source loading slice") {
    val manifest =
      NodeFs.readFileSync("packages/cosmoc/cosmo.json", "utf8").asInstanceOf[String]

    assert(manifest.contains("\"std/path_fs.cos\""))
    assert(manifest.contains("\"driver/config.cos\""))
    assert(manifest.contains("\"source/source.cos\""))
  }

  test("path fs docs keep filesystem support behind std and extern boundaries") {
    val std = NodeFs.readFileSync("docs/cosmo0/std.typ", "utf8").asInstanceOf[String]
    val runtime =
      NodeFs.readFileSync("docs/cosmo0/runtime.typ", "utf8").asInstanceOf[String]
    val delta = NodeFs
      .readFileSync(
        "openspec/specs/core0-path-fs/spec.md",
        "utf8",
      )
      .asInstanceOf[String]
    val stageDelta = NodeFs
      .readFileSync(
        "openspec/specs/core0-stage-capability-registry/spec.md",
        "utf8",
      )
      .asInstanceOf[String]

    assert(std.contains("`core0.path-fs`"))
    assert(std.contains("Result[String, IoError]"))
    assert(runtime.contains("cosmo0_runtime::read_file"))
    assert(stageDelta.contains("cosmo0.stage.missing-capability"))
    assert(delta.contains("`Path`, `IoError`, and `Fs` are not registered descriptor families"))
  }
end Cosmo1Stage1PathFsTest
