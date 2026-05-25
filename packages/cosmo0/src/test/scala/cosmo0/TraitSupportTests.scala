package cosmo0

class TraitSupportTests extends munit.FunSuite:
  test("non-generic trait impl methods lower as concrete type methods"):
    val lowered = Cosmo0().lower(
      """trait Drop {
        |  def drop(&self): Unit
        |}
        |
        |class Owned {
        |  val id: i32
        |}
        |
        |impl Drop for Owned {
        |  def drop(&self): Unit = {}
        |}
        |
        |def call_drop(value: Owned): Unit = {
        |  value.drop()
        |}
        |""".stripMargin,
    )

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"trait impl lowering failed with diagnostics: ${lowered.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(
      rendered.contains(
        "fn @Owned.drop drop(%self self: &Owned) -> Unit owner @Owned",
      ),
    )
    assert(rendered.contains("method_call %value.drop() -> Unit"))

  test("Drop impls emit C++ destructors that call lowered drop methods"):
    val compiled = Cosmo0().compile(
      """trait Drop {
        |  def drop(&self): Unit
        |}
        |
        |class Owned {
        |  val id: i32
        |}
        |
        |impl Drop for Owned {
        |  def drop(&self): Unit = {}
        |}
        |
        |def make_owned(): Owned = {
        |  Owned(1)
        |}
        |""".stripMargin,
    )

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"trait impl C++ emission failed with diagnostics: ${compiled.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val source = compiled.value.get.output
    val forwardDeclIndex =
      source.indexOf("inline void Owned_drop(const Owned * self);")
    val structIndex = source.indexOf("struct Owned {")
    assert(forwardDeclIndex >= 0, source)
    assert(structIndex >= 0, source)
    assert(
      forwardDeclIndex < structIndex,
      "drop forward declaration must appear before the struct destructor",
    )
    assert(source.contains("~Owned() {"))
    assert(source.contains("mutable bool __cosmo0_drop_active = true;"))
    assert(source.contains("Owned(const Owned &other)"))
    assert(source.contains("Owned(Owned &&other) noexcept"))
    assert(source.contains("Owned_drop(this);"))
    assert(source.contains("inline void Owned_drop(const Owned * self)"))

  test("trait impls must implement the declared method signatures"):
    val mismatch = Cosmo0().check(
      """trait Drop {
        |  def drop(&self): Unit
        |}
        |
        |class Owned {}
        |
        |impl Drop for Owned {
        |  def drop(&mut self): Unit = {}
        |}
        |""".stripMargin,
    )

    assertEquals(mismatch.phase, Phase.Check)
    assertEquals(mismatch.status, PhaseStatus.Failed)
    assert(
      mismatch.diagnostics.exists(
        _.code == "cosmo0.type.impl-signature-mismatch",
      ),
      s"missing signature mismatch diagnostic in ${mismatch.diagnostics.map(_.code)}",
    )

    val missing = Cosmo0().check(
      """trait Drop {
        |  def drop(&self): Unit
        |}
        |
        |class Owned {}
        |
        |impl Drop for Owned {}
        |""".stripMargin,
    )

    assertEquals(missing.phase, Phase.Check)
    assertEquals(missing.status, PhaseStatus.Failed)
    assert(
      missing.diagnostics.exists(_.code == "cosmo0.type.impl-missing-method"),
      s"missing method diagnostic in ${missing.diagnostics.map(_.code)}",
    )
