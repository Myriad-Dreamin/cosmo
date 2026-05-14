package cosmo

class Cosmo1LaterCommandTest extends TestBase:
  private val sourceFiles = List(
    "packages/cosmoc/src/core0/command.cos",
    "packages/cosmoc/src/core0/command_test.cos",
    "packages/cosmoc/src/link/command.cos",
  )

  test("cosmoc later command source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse later command source: $path")
    }
  }

  test("cosmoc Stage 1 manifest does not require command support") {
    val manifest =
      NodeFs.readFileSync("packages/cosmoc/cosmo.json", "utf8").asInstanceOf[String]

    assert(!manifest.contains("\"core0/command.cos\""))
    assert(!manifest.contains("\"core0/command_test.cos\""))
    assert(!manifest.contains("\"link/command.cos\""))
  }

  test("command docs and specs keep process execution behind std and extern boundaries") {
    val std = NodeFs.readFileSync("docs/cosmo0/std.typ", "utf8").asInstanceOf[String]
    val runtime =
      NodeFs.readFileSync("docs/cosmo0/runtime.typ", "utf8").asInstanceOf[String]
    val pkg = NodeFs.readFileSync("docs/cosmo0/package.typ", "utf8").asInstanceOf[String]
    val delta = NodeFs
      .readFileSync(
        "openspec/changes/add-core0-command/specs/core0-command/spec.md",
        "utf8",
      )
      .asInstanceOf[String]
    val externDelta = NodeFs
      .readFileSync(
        "openspec/changes/add-core0-command/specs/cosmo0-extern-abi-hooks/spec.md",
        "utf8",
      )
      .asInstanceOf[String]
    val stageDelta = NodeFs
      .readFileSync(
        "openspec/changes/add-core0-command/specs/core0-stage-capability-registry/spec.md",
        "utf8",
      )
      .asInstanceOf[String]

    assert(std.contains("`core0.command`"))
    assert(std.contains("Result[CommandResult, CommandError]"))
    assert(std.contains("shell command strings are not part of the initial standard surface"))
    assert(runtime.contains("cosmo0_runtime::command_run"))
    assert(runtime.contains("`Command`, `Process`, `Shell`, `ExitStatus`, `Stdout`, or `Stderr` descriptor family"))
    assert(pkg.contains("The `cosmo1.stage1` profile must continue to validate when `core0.command` is unavailable"))
    assert(delta.contains("Command APIs Remain Std-Owned"))
    assert(delta.contains("`Command`, `CommandResult`, `CommandError`, `Process`, and `Shell` are not registered descriptor families"))
    assert(externDelta.contains("::cosmo0_runtime::command_run"))
    assert(stageDelta.contains("core0.command is not required by Stage 1"))
    assert(stageDelta.contains("cosmo0.stage.missing-capability"))
  }
end Cosmo1LaterCommandTest
