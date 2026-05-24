package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object NlohmannJsonDependency:
  val repoUrl: String = "https://github.com/nlohmann/json.git"
  val version: String = "v3.11.3"
  val revision: String = "9cca280a4d0ccf0c08f47a99aa71d1b0e52f8d03"
  val rootDir: String = "target/cosmo/externals/json"
  val includeDir: String = s"$rootDir/single_include"

  private val externalsDir = "target/cosmo/externals"
  private val installLockDir = s"$rootDir.lock"
  private val gitDir = s"$rootDir/.git"
  private val headerPath = s"$includeDir/nlohmann/json.hpp"
  private val installLockAttempts = 480
  private val installLockWaitMillis = 250

  def includeArg: String =
    ensureAvailable()
    s"-I$includeDir"

  def ensureAvailable(): Unit =
    if DependencyNodeFs.existsSync(headerPath) then return

    DependencyNodeFs.mkdirSync(
      externalsDir,
      js.Dynamic.literal(recursive = true),
    )
    if !acquireInstallLock() then return

    try ensureAvailableUnderLock()
    finally releaseInstallLock()

  private def ensureAvailableUnderLock(): Unit =
    if DependencyNodeFs.existsSync(headerPath) then return

    if DependencyNodeFs.existsSync(rootDir) && !DependencyNodeFs.existsSync(
        gitDir,
      )
    then
      throw new IllegalStateException(
        s"nlohmann/json dependency path exists but is not a git checkout: $rootDir",
      )

    if !DependencyNodeFs.existsSync(gitDir) then clonePinnedDependency()
    if !hasPinnedRevision then fetchPinnedTag()

    runGit(
      js.Array("-C", rootDir, "checkout", "--detach", "--force", revision),
      s"checkout nlohmann/json $version ($revision)",
    )

    if DependencyNodeFs.existsSync(headerPath) then return

    throw new IllegalStateException(
      s"nlohmann/json checkout did not provide $headerPath at $version ($revision)",
    )

  private def acquireInstallLock(): Boolean =
    var attempt = 0
    while attempt < installLockAttempts do
      try
        DependencyNodeFs.mkdirSync(installLockDir, js.Dynamic.literal())
        return true
      catch
        case _: Throwable =>
          if DependencyNodeFs.existsSync(headerPath) then return false
          sleep(installLockWaitMillis)
          attempt += 1

    throw new IllegalStateException(
      s"timed out waiting for nlohmann/json dependency lock: $installLockDir",
    )

  private def releaseInstallLock(): Unit =
    DependencyNodeFs.rmSync(
      installLockDir,
      js.Dynamic.literal(recursive = true, force = true),
    )

  private def sleep(milliseconds: Int): Unit =
    val buffer = js.Dynamic.newInstance(js.Dynamic.global.SharedArrayBuffer)(4)
    val array = js.Dynamic.newInstance(js.Dynamic.global.Int32Array)(buffer)
    js.Dynamic.global.Atomics.applyDynamic("wait")(array, 0, 0, milliseconds)

  private def clonePinnedDependency(): Unit =
    runGit(
      js.Array(
        "clone",
        "--filter=blob:none",
        "--depth=1",
        "--branch",
        version,
        "--single-branch",
        repoUrl,
        rootDir,
      ),
      s"clone nlohmann/json $version",
    )

  private def fetchPinnedTag(): Unit =
    runGit(
      js.Array(
        "-C",
        rootDir,
        "fetch",
        "--filter=blob:none",
        "--depth=1",
        "origin",
        "tag",
        version,
      ),
      s"fetch nlohmann/json $version",
    )

  private def hasPinnedRevision: Boolean =
    val result = DependencyNodeSpawnSync(
      "git",
      js.Array("-C", rootDir, "cat-file", "-e", s"$revision^{commit}"),
      js.Dynamic.literal(encoding = "utf8", stdio = "pipe"),
    )
    result.status.toOption.contains(0)

  private def runGit(args: js.Array[String], action: String): Unit =
    val result = DependencyNodeSpawnSync(
      "git",
      args,
      js.Dynamic.literal(encoding = "utf8", stdio = "pipe"),
    )
    if result.status.toOption.contains(0) then return

    val output = List(
      s"failed to $action",
      s"command: git ${args.toList.mkString(" ")}",
      s"status: ${result.status.toOption.map(_.toString).getOrElse("unknown")}",
      result.stdout.toOption.map(_.trim).getOrElse(""),
      result.stderr.toOption.map(_.trim).getOrElse(""),
      result.error.toOption.map(_.message).getOrElse(""),
    ).filter(_.nonEmpty).mkString("\n")
    throw new IllegalStateException(output)

@js.native
@JSImport("fs", JSImport.Namespace)
private object DependencyNodeFs extends js.Object:
  def existsSync(path: String): Boolean = js.native
  def mkdirSync(path: String, options: js.Any): Unit = js.native
  def rmSync(path: String, options: js.Any): Unit = js.native

@js.native
@JSImport("child_process", "spawnSync")
private object DependencyNodeSpawnSync extends js.Object:
  def apply(
      command: String,
      args: js.Array[String],
      options: js.Any,
  ): DependencyNodeSpawnSyncResult =
    js.native

@js.native
private trait DependencyNodeSpawnSyncResult extends js.Object:
  val status: js.UndefOr[Int] = js.native
  val stdout: js.UndefOr[String] = js.native
  val stderr: js.UndefOr[String] = js.native
  val error: js.UndefOr[js.Error] = js.native
