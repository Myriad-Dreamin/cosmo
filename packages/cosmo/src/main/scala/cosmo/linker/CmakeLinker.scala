package cosmo.linker

import cosmo.system._
import cosmo.{Package, Transpiler, FileId}
import cosmo.{debugln, logln}

import scala.scalajs.js

class CmakeLinker(system: CosmoSystem) extends Linker {
  lazy val buildDir = "cmake-build-relwithdebinfo";

  def writeIfDiff(path: String, content: String): Unit =
    cosmo.linker.writeIfDiff(system, path, content)

  def assemblePkg(
      pkg: cosmo.Package,
      t: cosmo.Transpiler,
      relReleaseDir: String,
  ): Unit = {
    val start = System.currentTimeMillis()
    headOnlyPkg(
      pkg,
      t,
      relReleaseDir,
      writeIfDiff,
    );

    writeIfDiff(
      s"$relReleaseDir/CMakeLists.txt",
      s"""
include(packageOnly.cmake)

add_library(cosmo_std INTERFACE)
target_include_directories(cosmo_std INTERFACE .)
""",
    );

    writeIfDiff(
      s"$relReleaseDir/packageOnly.cmake",
      s"""

""",
    );
    debugln(s"Assembly time: ${System.currentTimeMillis() - start}ms")
  }

  def compile(
      path: String,
      t: cosmo.Transpiler,
      relReleaseDir: String,
  ): Option[String] = {
    val start = System.currentTimeMillis()
    val src = system.readFile(path)
    val generated = t.transpile(src).map { case (content, noCore) =>
      var suf = if (noCore) "/lang" else ""
      s"""#include <cosmo/std/src/prelude${suf}.h> // IWYU pragma: keep\n\n${content}"""
    }

    var nlJsonDir = system
      .absPath("target/cosmo/externals/json/single_include")
      .replace("\\", "\\\\")

    var releaseDir =
      system.absPath(relReleaseDir).replace("\\", "\\\\")

    var includeFlags = js.Array(
      s"/I$nlJsonDir",
      s"/I$releaseDir",
    )

    val fileName = path.substring(0, path.length - 4)
    val destDir = "package-less"
    val destPath = destDir + "/" + fileName + ".cc"
    val dirPath = destPath.substring(0, destPath.lastIndexOf("/"))

    generated.flatMap { content =>
      system.mkdir(releaseDir + "/" + dirPath)
      system.writeFile(releaseDir + "/" + destPath, content)

      writeIfDiff(
        s"$relReleaseDir/packageOnly.cmake",
        s"""
add_executable(cosmo-user-prog $destPath)
target_link_libraries(cosmo-user-prog PUBLIC cosmo_std)
""",
      );

      val target = "cosmo-user-prog"

      val cmCommand = "cmake"
      val cmArgs = js.Array(
        "--build",
        buildDir,
        "--config",
        "RelWithDebInfo",
        "--target",
        target,
      )

      val result = cosmo.NodeChildProcess.spawnSync(
        cmCommand,
        cmArgs,
        js.Dynamic.literal(
          encoding = "utf8",
          stdio = "pipe",
          // stdio = js.Tuple3("pipe", 2, "pipe"),
        ),
      )

      def programPath = {
        // todo: this only works with ninja
        Some(s"$buildDir/$relReleaseDir/$target.exe")
      }

      debugln(s"Compilation time: ${System.currentTimeMillis() - start}ms")
      result.status.toOption match {
        case Some(0) => programPath
        case None    => programPath
        case Some(status) =>
          println(result.stdout.toString().trim())
          println(result.stderr.toString().trim())
          println(result.error.toString().trim())
          println(s"Compilation failed: cmake.exe Exit with status: ${status}")
          None
      }
    }
  }
}
