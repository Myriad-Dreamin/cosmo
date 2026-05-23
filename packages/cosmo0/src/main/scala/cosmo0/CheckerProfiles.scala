package cosmo0

final case class CheckerProfile(
    id: String,
    owner: String,
    inputArtifact: String,
    artifactKind: String,
    diagnosticNamespace: String,
    supportedFeatures: Set[String],
    rejectedFeatures: Map[String, String],
):
  def supports(feature: String): Boolean =
    supportedFeatures.contains(feature)

  def rejects(feature: String): Boolean =
    rejectedFeatures.contains(feature)

  def rejectedFeatureCode(feature: String): String =
    rejectedFeatures.getOrElse(feature, CheckerProfiles.UnsupportedFeatureCode)

  def summary: String =
    s"$id|$inputArtifact|$artifactKind|$diagnosticNamespace"

object CheckerProfiles:
  val CosmocBasicExprId = "cosmoc.basic-expr"
  val Cosmo0SubsetId = "cosmo0.subset"
  val MlttCoreId = "mltt.core"

  val UnsupportedFeatureCode = "cosmo.type.unsupported-feature"
  val UnsupportedDependentPatternCode = "cosmo.type.unsupported-dependent-pattern"
  val UnsupportedEffectRowCode = "cosmo.type.unsupported-effect-row"
  val UnsupportedTraitConstraintCode = "cosmo.type.unsupported-trait-constraint"
  val UnsupportedObjectDispatchCode = "cosmo.type.unsupported-object-dispatch"
  val UnsupportedCppImportCode = "cosmo.type.unsupported-cpp-import"
  val ImplementationLimitCode = "cosmo.type.implementation-limit"

  val EffectsFeature = "effects"
  val TraitsFeature = "traits"
  val ObjectDispatchFeature = "object-dispatch"
  val DependentPatternsFeature = "dependent-patterns"
  val CppImportsFeature = "cpp-imports"

  val CosmocBasicExpr: CheckerProfile =
    CheckerProfile(
      CosmocBasicExprId,
      "packages/cosmoc/src/types",
      "syntax-arenas+name-resolution+declaration-signatures",
      "typed-expression-map",
      "cosmo.type",
      Set(
        "primitive-expressions",
        "locals",
        "functions",
        "classes",
        "methods",
        "control-flow",
        "standard-intrinsics",
      ),
      Map(
        EffectsFeature -> UnsupportedEffectRowCode,
        TraitsFeature -> UnsupportedTraitConstraintCode,
        DependentPatternsFeature -> UnsupportedDependentPatternCode,
      ),
    )

  val Cosmo0Subset: CheckerProfile =
    CheckerProfile(
      Cosmo0SubsetId,
      "packages/cosmo0/src/main/scala/cosmo0/SourceTyper.scala",
      "cosmo0-untyped-module",
      "typed-module",
      "cosmo0.type",
      Set(
        "cosmo0-declarations",
        "cosmo0-expressions",
        "cosmo0-control-flow",
        "cosmo0-standard-generics",
        "cosmo0-cpp-namespace-imports",
      ),
      Map(
        EffectsFeature -> UnsupportedEffectRowCode,
        DependentPatternsFeature -> UnsupportedDependentPatternCode,
      ),
    )

  val MlttCore: CheckerProfile =
    CheckerProfile(
      MlttCoreId,
      "packages/cosmoc/src/types/mltt",
      "syntax-arenas+name-resolution+declaration-signatures",
      "mltt-core-term",
      "cosmo.type",
      Set(
        "mltt-core-terms",
        "universes",
        "pi-types",
        "conversion",
      ),
      Map(
        EffectsFeature -> UnsupportedEffectRowCode,
        TraitsFeature -> UnsupportedTraitConstraintCode,
        ObjectDispatchFeature -> UnsupportedObjectDispatchCode,
        DependentPatternsFeature -> UnsupportedDependentPatternCode,
        CppImportsFeature -> UnsupportedCppImportCode,
      ),
    )

  val all: List[CheckerProfile] =
    List(CosmocBasicExpr, Cosmo0Subset, MlttCore)

  def byId(id: String): Option[CheckerProfile] =
    all.find(_.id == id)

  def firstUnsupportedFeatureForUnavailableProfile(profile: CheckerProfile): String =
    if profile.rejects(ObjectDispatchFeature) then ObjectDispatchFeature
    else if profile.rejects(EffectsFeature) then EffectsFeature
    else profile.rejectedFeatures.keys.toList.sorted.headOption.getOrElse("profile-implementation")

  def unsupportedDiagnostic(
      profile: CheckerProfile,
      feature: String,
      span: Option[SourceSpan] = None,
  ): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      profile.rejectedFeatureCode(feature),
      s"checker profile ${profile.id} does not support feature $feature",
      span,
    )

  def unknownProfileDiagnostic(id: String): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      "cosmo.type.unknown-checker-profile",
      s"unknown checker profile $id",
    )
