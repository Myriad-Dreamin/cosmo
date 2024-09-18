package cosmo
import scala.scalajs.js

class SelfHostTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var src =
      cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    compiler.preloadPackage("std");

    val prog = compiler.mayGetExecutable(path);
    prog.foreach(
      NodeChildProcess.execSync(_, js.Dynamic.literal(stdio = "inherit")),
    )
  }

  test("CompileDriver/callNode") {
    runTestOnFile("samples/CompileDriver/callNode.cos")
  }
end SelfHostTest
