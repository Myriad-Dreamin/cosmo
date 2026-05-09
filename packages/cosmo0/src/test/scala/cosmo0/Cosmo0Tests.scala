package cosmo0

class Cosmo0Tests extends munit.FunSuite:
  test("facade can be instantiated independently"):
    val compiler = Cosmo0()
    assertNotEquals(compiler, null)

  test("parse accepts source text and returns a parsed module"):
    val result = Cosmo0().parse("val answer = 42")

    assertEquals(result.phase, Cosmo0Phase.Parse)
    assertEquals(result.status, Cosmo0PhaseStatus.Succeeded)
    assert(result.value.nonEmpty)
    assertEquals(result.value.get.source.name, "<memory>")
    assert(result.diagnostics.isEmpty)

  test("parse failures return structured diagnostics"):
    val result = Cosmo0().parse("val =")

    assertEquals(result.phase, Cosmo0Phase.Parse)
    assertEquals(result.status, Cosmo0PhaseStatus.Failed)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.parse.failed")
    assert(result.diagnostics.head.span.nonEmpty)

  test("check returns a structured pending result"):
    val result = Cosmo0().check("val answer = 42")

    assertEquals(result.phase, Cosmo0Phase.Check)
    assertEquals(result.status, Cosmo0PhaseStatus.Pending)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.check.pending")

  test("compile returns a structured unsupported result"):
    val result = Cosmo0().compile("val answer = 42")

    assertEquals(result.phase, Cosmo0Phase.Compile)
    assertEquals(result.status, Cosmo0PhaseStatus.Unsupported)
    assert(result.value.isEmpty)
    assertEquals(result.diagnostics.head.code, "cosmo0.compile.unsupported")
