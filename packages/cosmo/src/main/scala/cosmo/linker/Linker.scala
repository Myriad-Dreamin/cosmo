package cosmo.linker

import cosmo.{Transpiler, Package}

trait Linker {
  def assemblePkg(pkg: Package, t: Transpiler, releaseDir: String): Unit

  def compile(path: String, t: Transpiler, releaseDir: String): Option[String]
}
