package cosmo.linker

import cosmo.system.CosmoSystem
import cosmo.{Package, Transpiler, FileId}

def headOnlyPkg(
    pkg: Package,
    t: Transpiler,
    releaseDir: String,
    write: (String, String) => Unit,
) = {
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

      var fileName = pathWithoutExt.substring(
        pathWithoutExt.lastIndexOf("/") + 1,
      )

      var subIdentifier =
        (identifier + "_" + pathWithoutExt.replace("/", "_")).toUpperCase

      var generated =
        t.transpile(src.source, Some(src.fid)).map { case (content, noCore) =>
          s"""#ifndef ${subIdentifier}_H\n#define ${subIdentifier}_H\n\n""" + content + s"\n\n#endif // ${subIdentifier}_H\n"
        }

      var dirPath = destDir + pathWithoutExt.substring(
        0,
        pathWithoutExt.lastIndexOf("/"),
      )
      generated.map(content =>
        write(hPath, content)
        write(
          ccPath,
          s"#include \"$fileName.h\" // IWYU pragma: keep\n",
        ),
      )
      sources = sources :+ ccPath
    }
  }

  (destDir, sources)
}
