package cosmo.linker

import cosmo.system.CosmoSystem
import cosmo.artifact._
import cosmo.{Package, Transpiler, FileId}

def headOnlyPkg(
    pkg: Package,
    t: Transpiler,
    releaseDir: String,
    write: (String, String) => Unit,
) = {
  val destDir = pkg.destDir(releaseDir)
  var sources = List[String]()
  val identifier = (pkg.namespace + "_" + pkg.name).toUpperCase
  for (src <- pkg.sources.values) {
    if (!src.fid.path.endsWith("staging.cos")) {
      val s = t.transpileByFid(src.fid)

      // .cos -> .cc, .h
      var stripPath = src.fid.stripPath
      var fileName = stripPath.substring(stripPath.lastIndexOf("/") + 1)
      var absStripPath = destDir + stripPath

      var subIdentifier =
        (identifier + "_" + stripPath.replace("/", "_")).toUpperCase
      var hValue = s.map { case (content, env) =>
        s"""#ifndef ${subIdentifier}_H\n#define ${subIdentifier}_H\n\n""" + content + s"\n\n#endif // ${subIdentifier}_H\n"
      }
      var cValue = Some(s"#include \"$fileName.h\" // IWYU pragma: keep\n")
      var dValue = s
        .map { case (_, env) =>
          env.deps.map(_._1.toString).mkString("\n")
        }

      hValue.map(write(absStripPath + ".h", _))
      cValue.map(write(absStripPath + ".cc", _))
      dValue.map(write(absStripPath + ".d", _))
      // irValue.map(write(absStripPath + ".ir.cos", _))
      sources = sources :+ absStripPath + ".cc"
    }
  }

  (destDir, sources)
}
