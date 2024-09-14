package cosmo.linker

import cosmo.Package

trait Linker {
  def assemblePkg(
      pkg: Package,
      loader: String => Option[(String, Boolean)],
      releaseDir: String,
  ): Unit

  def compile(
      path: String,
      loader: String => Option[(String, Boolean)],
      releaseDir: String,
  ): Option[String]
}
