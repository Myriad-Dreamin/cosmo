package cosmo.linker

import cosmo.system._
import cosmo.Package

import scala.scalajs.js

class MsvcLinker(system: CosmoSystem) extends Linker {

  lazy val installationPath = findInstallationPath
  lazy val systemIncludePath = findSystemIncludePath
  lazy val systemLibPath = findSystemLibPath
  lazy val version = findVersion
  lazy val clPath = findClPath
  lazy val windowsSdkIncludePath = findWindowsSdkIncludePath
  lazy val sdkVersion = findSdkVersion

  def assemblePkg(
      pkg: Package,
      loader: String => Option[String],
      releaseDir: String,
  ): Unit = {
    val destDir = releaseDir + "/" + pkg.namespace + "/" + pkg.name + "/src"
    var sources = List[String]()
    val identifier = (pkg.namespace + "_" + pkg.name).toUpperCase
    for ((path, src) <- pkg.sources) {
      if (!path.endsWith("staging.cos")) {
        // .cos -> .cc, .h
        var pathWithoutExt = path.substring(0, path.length - 4)
        var ccPath = destDir + pathWithoutExt + ".cc"
        var hPath = destDir + pathWithoutExt + ".h"
        // println(s"Preloading $path => $hPath");

        var dirPath = destDir + pathWithoutExt.substring(
          0,
          pathWithoutExt.lastIndexOf("/"),
        )
        system.mkdir(dirPath)
        var fileName = pathWithoutExt.substring(
          pathWithoutExt.lastIndexOf("/") + 1,
        )

        var subIdentifier =
          (identifier + "_" + pathWithoutExt.replace("/", "_")).toUpperCase

        var generated = loader(src.source).map { content =>
          s"""#ifndef ${subIdentifier}_H\n#define ${subIdentifier}_H\n\n""" + content + s"\n\n#endif // ${subIdentifier}_H\n"
        }

        generated.map(system.writeFile(hPath, _))
        generated.map(_ =>
          system.writeFile(
            ccPath,
            s"#include \"$fileName.h\" // IWYU pragma: keep\n",
          ),
        )
        sources = sources :+ ccPath
      }
    }

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

    var cosmoRtDir = system
      .absPath("target/cosmo/externals/cosmo-rt/include")
      .replace("\\", "\\\\")

    var nlJsonDir = system
      .absPath("target/cosmo/externals/json/single_include")
      .replace("\\", "\\\\")

    var includeFlags = s"-I\"$cosmoRtDir\" -I\"$nlJsonDir\""

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
      loader: String => Option[String],
      releaseDir: String,
  ): Option[String] = {
    val src = system.readFile(path)
    val generated = loader(src).map { content =>
      // s"""#include "cosmo/std/index.h"\n\n""" + content
      content
    }

    var cosmoRtDir = system
      .absPath("target/cosmo/externals/cosmo-rt/include")
      .replace("\\", "\\\\")

    var nlJsonDir = system
      .absPath("target/cosmo/externals/json/single_include")
      .replace("\\", "\\\\")

    var releaseDir =
      system.absPath("target/cosmo/release").replace("\\", "\\\\")

    var includeFlags = s"/I\"$cosmoRtDir\" /I\"$nlJsonDir\" /I\"$releaseDir\""

    var linkFlags =
      s"/LIBPATH:\"$sdkPath/lib/${sdkVersion}/ucrt/x64\" /LIBPATH:\"$sdkPath/lib/${sdkVersion}/um/x64\" /LIBPATH:\"$systemLibPath\""

    val pathWithoutExt = path.substring(0, path.length - 4)

    val programPath = generated.map { content =>
      val destDir = releaseDir + "/package-less"
      val destPath = destDir + "/" + path.substring(0, path.length - 4) + ".cc"
      val dirPath = destPath.substring(0, destPath.lastIndexOf("/"))
      system.mkdir(dirPath)
      system.writeFile(destPath, content)

      val programPath = s"$destDir\\$pathWithoutExt.exe"

      if (cosmo.NodeFs.existsSync(programPath).asInstanceOf[Boolean]) {
        cosmo.NodeFs.unlinkSync(programPath)
      }

      val clCommand =
        s""""$clPath" /std:c++17 /EHsc /I"$windowsSdkIncludePath" /I"$systemIncludePath" $includeFlags /I. /Fe:"$programPath" "$destPath" /link $linkFlags"""

      val result = cosmo.NodeChildProcess.execSync(
        clCommand,
        js.Dynamic.literal(
          encoding = "utf8",
          stdio = "pipe",
          cwd = dirPath,
        ),
      )

      if (result.status.asInstanceOf[Int] != 0) {
        throw new Exception(s"Compilation failed: ${result.stderr}")
      }

      programPath
    }

    programPath
  }

  def findInstallationPath: String = {

    val vswherePath =
      "C:\\Program Files (x86)\\Microsoft Visual Studio\\Installer\\vswhere.exe"
    if (!cosmo.NodeFs.existsSync(vswherePath).asInstanceOf[Boolean]) {
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
    if (!cosmo.NodeFs.existsSync(versionPath).asInstanceOf[Boolean]) {
      throw new Exception("Microsoft.VCToolsVersion.default.txt not found")
    }

    cosmo.NodeFs.readFileSync(versionPath, "utf8").asInstanceOf[String].trim
  }

  def findClPath: String = {
    val clPath =
      installationPath + s"/VC/Tools/MSVC/${version}/bin/Hostx64/x64/cl.exe"

    if (!cosmo.NodeFs.existsSync(clPath).asInstanceOf[Boolean]) {
      throw new Exception("cl.exe not found")
    }

    clPath
  }

  def findSystemIncludePath: String = {

    val systemIncludePath =
      installationPath + s"/VC/Tools/MSVC/${version}/include"

    if (!cosmo.NodeFs.existsSync(systemIncludePath).asInstanceOf[Boolean]) {
      throw new Exception("system include path not found")
    }

    systemIncludePath
  }

  def findSystemLibPath: String = {

    val systemLibPath =
      installationPath + s"/VC/Tools/MSVC/${version}/lib/x64"

    if (!cosmo.NodeFs.existsSync(systemLibPath).asInstanceOf[Boolean]) {
      throw new Exception("system lib path not found")
    }

    systemLibPath
  }

  val sdkPath = "C:\\Program Files (x86)\\Windows Kits\\10"

  def findSdkVersion = {

    val manifestPath = sdkPath + "/SDKManifest.xml"
    if (!cosmo.NodeFs.existsSync(manifestPath).asInstanceOf[Boolean]) {
      throw new Exception("SDKManifest.xml not found")
    }

    val manifest: String =
      cosmo.NodeFs.readFileSync(manifestPath, "utf8").asInstanceOf[String]

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
