package cosmo.linker

import cosmo.{Transpiler, Package}
import cosmo.NodePath

trait Linker {
  def assemblePkg(pkg: Package, t: Transpiler, releaseDir: String): Unit

  def compile(path: String, t: Transpiler, releaseDir: String): Option[String]
}

def writeIfDiff(
    system: cosmo.system.CosmoSystem,
    path: String,
    content: String,
): Unit = {
  if (system.exists(path)) {
    val oldContent = system.readFile(path)
    if (oldContent != content) {
      system.writeFile(path, content)
    }
  } else {
    system.mkdir(NodePath.dirname(path))
    system.writeFile(path, content)
  }
}
