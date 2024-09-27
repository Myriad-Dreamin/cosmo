package cosmo.linker

import cosmo.system._
import cosmo.Package
import cosmo.Transpiler
import cosmo.FileId

import scala.scalajs.js

class MsvcLinker(system: CosmoSystem) extends Linker {

  lazy val installationPath = findInstallationPath
  lazy val systemIncludePath = findSystemIncludePath
  lazy val systemLibPath = findSystemLibPath
  lazy val version = findVersion
  lazy val clPath = findClPath
  lazy val windowsSdkIncludePath = findWindowsSdkIncludePath
  lazy val sdkVersion = findSdkVersion

  def writeIfDiff(path: String, content: String): Unit =
    cosmo.linker.writeIfDiff(system, path, content)

  def assemblePkg(
      pkg: Package,
      t: Transpiler,
      relReleaseDir: String,
  ): Unit = {
    val (destDir, sources) = headOnlyPkg(pkg, t, relReleaseDir, writeIfDiff)
    val identifier = (pkg.namespace + "_" + pkg.name).toUpperCase

    // generate all in one file
    var allHPath = destDir + "/../index.h"
    var allContent = sources
      .map(path => s"""#include "./src${path.substring(destDir.length)}"""")
      .mkString(
        s"#ifndef ${identifier}_H\n#define ${identifier}_H\n\n",
        "\n",
        s"\n\n#endif // ${identifier}_H\n",
      )
    system.writeFile(allHPath, allContent)

    var allCCPath = destDir + "/../index.cc"
    var allCCContent = s"""#include "./index.h" // IWYU pragma: keep\n"""
    system.writeFile(allCCPath, allCCContent)

    var absReleaseDir = system.absPath(destDir + "/..").replace("\\", "\\\\")

    var nlJsonDir = system
      .absPath("target/cosmo/externals/json/single_include")
      .replace("\\", "\\\\")

    var releaseRoot =
      system.absPath(relReleaseDir).replace("\\", "\\\\")

    var includeFlags = s"-I$nlJsonDir -I$releaseRoot"

    var compilationCommandsPath = destDir + "/../compile_commands.json"
    var compilationCommands = (sources :+ allCCPath)
      .map(path => path.substring(destDir.length - 3))
      .map(path =>
        s"""{"directory": "$absReleaseDir", "command": "clang -c $includeFlags -I. -std=c++17 -o """ + path + ".o " + path + """", "file": """" + path + """"}""",
      )
      .mkString("[\n", ",\n", "\n]\n")
    system.writeFile(compilationCommandsPath, compilationCommands)
  }

  def compile(
      path: String,
      t: Transpiler,
      relReleaseDir: String,
  ): Option[String] = {
    val generated = t.transpileByPath(path).map { case (content, env) =>
      var suf = if (env.noCore) "/lang" else ""
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

    var linkFlags = js.Array(
      s"/LIBPATH:$sdkPath/lib/${sdkVersion}/ucrt/x64",
      s"/LIBPATH:$sdkPath/lib/${sdkVersion}/um/x64",
      s"/LIBPATH:$systemLibPath",
    )

    val pathWithoutExt = path.substring(0, path.length - 4)

    generated.flatMap { content =>
      val destDir = releaseDir + "/package-less"
      val destPath = destDir + "/" + path.substring(0, path.length - 4) + ".cc"
      val dirPath = destPath.substring(0, destPath.lastIndexOf("/"))
      system.mkdir(dirPath)
      system.writeFile(destPath, content)

      val programPath = s"$destDir\\$pathWithoutExt.exe"

      if (system.exists(programPath).asInstanceOf[Boolean]) {
        system.unlink(programPath)
      }

      val clCommand = clPath
      val clArgs = js.Array(
        "/O2",
        "/std:c++17",
        "/EHsc",
        s"/I$windowsSdkIncludePath",
        s"/I$systemIncludePath",
      ) ++ includeFlags ++ js.Array(
        "/I.",
        s"/Fe:$programPath",
        destPath,
        "/link",
      ) ++ linkFlags

      val result = cosmo.NodeChildProcess.spawnSync(
        clCommand,
        clArgs,
        js.Dynamic.literal(
          encoding = "utf8",
          stdio = "pipe",
          // stdio = js.Tuple3("pipe", 2, "pipe"),
          cwd = dirPath,
        ),
      )

      result.status.toOption match {
        case Some(0) => Some(programPath)
        case None    => Some(programPath)
        case Some(status) =>
          println(result.stdout.toString().trim())
          println(s"Compilation failed: cl.exe Exit with status: ${status}")
          None
      }
    }
  }

  def findInstallationPath: String = {

    val vswherePath =
      "C:\\Program Files (x86)\\Microsoft Visual Studio\\Installer\\vswhere.exe"
    if (!system.exists(vswherePath).asInstanceOf[Boolean]) {
      throw new Exception("vswhere.exe not found")
    }

    val vswhere = cosmo.NodeChildProcess.execSync(
      s""""${vswherePath}"""",
      js.Dynamic.literal(encoding = "utf8"),
    )

    val vsWhereInfo = (vswhere: Any) match {
      case s: String => s
      case _         => ""
    }

    val installationPath = vsWhereInfo
      .split("\n")
      .find(_.contains("installationPath:"))
      .map(_.split(":", 2)(1).trim)

    if (installationPath.isEmpty) {
      throw new Exception(s"parse vswhere.exe failed: ${vsWhereInfo}")
    }

    // println(s"MSVC installation path: ${installationPath.get}")
    installationPath.get
  }

  def findVersion: String = {

    val versionPath =
      installationPath + "/VC/Auxiliary/Build/Microsoft.VCToolsVersion.default.txt"
    if (!system.exists(versionPath).asInstanceOf[Boolean]) {
      throw new Exception("Microsoft.VCToolsVersion.default.txt not found")
    }

    system.readFile(versionPath).trim
  }

  def findClPath: String = {
    val clPath =
      installationPath + s"/VC/Tools/MSVC/${version}/bin/Hostx64/x64/cl.exe"

    if (!system.exists(clPath).asInstanceOf[Boolean]) {
      throw new Exception("cl.exe not found")
    }

    clPath
  }

  def findSystemIncludePath: String = {

    val systemIncludePath =
      installationPath + s"/VC/Tools/MSVC/${version}/include"

    if (!system.exists(systemIncludePath).asInstanceOf[Boolean]) {
      throw new Exception("system include path not found")
    }

    systemIncludePath
  }

  def findSystemLibPath: String = {

    val systemLibPath =
      installationPath + s"/VC/Tools/MSVC/${version}/lib/x64"

    if (!system.exists(systemLibPath).asInstanceOf[Boolean]) {
      throw new Exception("system lib path not found")
    }

    systemLibPath
  }

  val sdkPath = "C:\\Program Files (x86)\\Windows Kits\\10"

  def findSdkVersion = {

    val manifestPath = sdkPath + "/SDKManifest.xml"
    if (!system.exists(manifestPath).asInstanceOf[Boolean]) {
      throw new Exception("SDKManifest.xml not found")
    }

    val manifest: String = system.readFile(manifestPath)

    // PlatformIdentity attribute is the version
    val identityStr = manifest
      .split("\n")
      .find(_.contains("PlatformIdentity"))
      .map(_.split("\"")(1).trim)

    val versionStr = identityStr
      .map(_.split(","))
      .map(_.find(_.contains("Version=")))
      .flatten
      .map(_.split("=")(1))

    // println(s"Windows SDK version: ${versionStr.get}")

    if (versionStr.isEmpty) {
      throw new Exception("parse SDKManifest.xml failed")
    }

    versionStr.get
  }

  def findWindowsSdkIncludePath: String = {

    val windowsSdkIncludePath =
      sdkPath + s"/include/${sdkVersion}/ucrt"

    windowsSdkIncludePath
  }
}
