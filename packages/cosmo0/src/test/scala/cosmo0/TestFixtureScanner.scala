package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object TestFixtureScanner:
  def filesUnder(root: String, include: String => Boolean): List[String] =
    if !ParserFixtureManifest.exists(root) then Nil
    else scan(root, include).sorted

  private def scan(path: String, include: String => Boolean): List[String] =
    val stats = TestFixtureNodeFs.statSync(path)
    if stats.isFile() then if include(path) then List(path) else Nil
    else if stats.isDirectory() then
      TestFixtureNodeFs
        .readdirSync(path)
        .toList
        .flatMap(name => scan(join(path, name), include))
    else Nil

  private def join(parent: String, child: String): String =
    if parent.endsWith("/") then s"$parent$child" else s"$parent/$child"

@js.native
@JSImport("node:fs", JSImport.Namespace)
private object TestFixtureNodeFs extends js.Object:
  def readdirSync(path: String): js.Array[String] = js.native
  def statSync(path: String): TestFixtureNodeStats = js.native

@js.native
private trait TestFixtureNodeStats extends js.Object:
  def isDirectory(): Boolean = js.native
  def isFile(): Boolean = js.native
