package cosmo.system

trait CosmoSystem {
  def readFile(path: String): String
  def readDir(path: String): List[String]

  def mkdir(path: String): Unit
  def writeFile(path: String, content: String): Unit

  def absPath(path: String): String
}
