package cosmo0

/** Static description of a checker implementation or experiment.
  *
  * A profile is deliberately metadata, not a dynamic dispatcher. It records the
  * artifact a checker expects as input, the artifact it returns, and the
  * feature set that callers may advertise or reject in diagnostics.
  *
  * Profile selection examples:
  *
  * {{{
  * // No package metadata:
  * //   checkerProfile = None
  * //   selected profile = mltt.dependent-patterns
  * //   implementation = MlttTyper over UntypedModule
  *
  * // Experimental package metadata:
  * //   "checkerProfile": "mltt.core"
  * //   selected profile = mltt.core
  * //   implementation = MlttTypeChecker.checkSources over MLTT directives
  *
  * // Direct test harness:
  * //   MlttTypeChecker.infer(store, context, term)
  * //   implementation = Scala MLTT core mirror in tyck/mltt/TypeChecker.scala
  * }}}
  *
  * Routing rule:
  *
  *   - Ordinary source executes through `MlttTyper`.
  *   - `mltt.core` and `mltt.dependent-patterns` source directives execute
  *     through `MlttTypeChecker`; the dependent-pattern profile invokes
  *     `DependentPatterns` as an MLTT extension.
  *   - Profiles without a concrete implementation remain unsupported checker
  *     results.
  */
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

/** Registry of type-checker profiles known to cosmo0.
  *
  * Language profile examples:
  *
  * {{{
  * cosmo0.subset:
  *   source declarations, expressions, classes, traits, impls, variants,
  *   standard generics, source match patterns, and C++ namespace imports.
  *
  * mltt.core:
  *   core terms such as Type0, Pi, Sigma, lambda, application, Eq, Refl,
  *   Nat, Vec, metavariables, and WHNF conversion.
  *
  * mltt.dependent-patterns:
  *   MLTT core terms plus constructor metadata and source pattern clauses
  *   elaborated to case-tree artifacts.
  * }}}
  *
  * Feature inference is intentionally simple: callers ask whether a profile
  * supports or rejects a named feature. The profile does not infer source
  * syntax by itself; concrete inference rules live in `MlttTyper`,
  * `MlttTypeChecker`, and `DependentPatterns`.
  */
object CheckerProfiles:
  val CosmocBasicExprId = "cosmoc.basic-expr"
  val Cosmo0SubsetId = "cosmo0.subset"
  val MlttCoreId = "mltt.core"
  val MlttDependentPatternsId = "mltt.dependent-patterns"

  val UnsupportedFeatureCode = "cosmo.type.unsupported-feature"
  val UnsupportedDependentPatternCode =
    "cosmo.type.unsupported-dependent-pattern"
  val UnsupportedEffectRowCode = "cosmo.type.unsupported-effect-row"
  val UnsupportedTraitConstraintCode = "cosmo.type.unsupported-trait-constraint"
  val UnsupportedObjectDispatchCode = "cosmo.type.unsupported-object-dispatch"
  val UnsupportedCppImportCode = "cosmo.type.unsupported-cpp-import"
  val ImplementationLimitCode = "cosmo.type.implementation-limit"

  val EffectsFeature = "effects"
  val AsyncFeature = "async"
  val TraitsFeature = "traits"
  val ObjectDispatchFeature = "object-dispatch"
  val DependentPatternsFeature = "dependent-patterns"
  val DependentPatternElaborationFeature = "dependent-pattern-elaboration"
  val CppImportsFeature = "cpp-imports"
  val MacrosFeature = "macros"
  val DeriveMacrosFeature = "derive-macros"
  val ReflectionFeature = "reflection"
  val StagingFeature = "staging"

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
      "packages/cosmo0/src/main/scala/cosmo0/tyck/mltt/Typer.scala",
      "cosmo0-untyped-module",
      "typed-module",
      "cosmo0.type",
      Set(
        "cosmo0-declarations",
        "cosmo0-expressions",
        "cosmo0-control-flow",
        "cosmo0-standard-generics",
        "cosmo0-cpp-namespace-imports",
        DependentPatternsFeature,
        DependentPatternElaborationFeature,
        MacrosFeature,
        DeriveMacrosFeature,
        ReflectionFeature,
      ),
      Map(
        EffectsFeature -> UnsupportedEffectRowCode,
      ),
    )

  val MlttCore: CheckerProfile =
    CheckerProfile(
      MlttCoreId,
      "packages/cosmo0/src/main/scala/cosmo0/tyck/mltt/TypeChecker.scala",
      "mltt-core-term+source-assertion-directives",
      "mltt-core-term",
      "cosmo.type",
      Set(
        "mltt-core-terms",
        "universes",
        "pi-types",
        "sigma-types",
        "equality-types",
        "let-reduction",
        "nat-metadata",
        "vec-metadata",
        "metavariables",
        "conversion",
        "whnf-conversion",
      ),
      Map(
        EffectsFeature -> UnsupportedEffectRowCode,
        AsyncFeature -> UnsupportedEffectRowCode,
        TraitsFeature -> UnsupportedTraitConstraintCode,
        ObjectDispatchFeature -> UnsupportedObjectDispatchCode,
        DependentPatternsFeature -> UnsupportedDependentPatternCode,
        CppImportsFeature -> UnsupportedCppImportCode,
        MacrosFeature -> UnsupportedFeatureCode,
        DeriveMacrosFeature -> UnsupportedFeatureCode,
        ReflectionFeature -> UnsupportedFeatureCode,
        StagingFeature -> UnsupportedFeatureCode,
      ),
    )

  val MlttDependentPatterns: CheckerProfile =
    CheckerProfile(
      MlttDependentPatternsId,
      "packages/cosmo0/src/main/scala/cosmo0/tyck/mltt/TypeChecker.scala",
      "mltt-core-term+constructor-metadata+source-pattern-clauses",
      "mltt-case-tree",
      "cosmo.type",
      Set(
        "mltt-core-terms",
        "universes",
        "pi-types",
        "sigma-types",
        "equality-types",
        "let-reduction",
        "nat-metadata",
        "vec-metadata",
        "metavariables",
        "conversion",
        "whnf-conversion",
        DependentPatternsFeature,
        DependentPatternElaborationFeature,
        MacrosFeature,
        DeriveMacrosFeature,
        ReflectionFeature,
        "inductive-family-constructors",
        "case-trees",
        "constructor-split-coverage",
      ),
      Map(
        EffectsFeature -> UnsupportedEffectRowCode,
        AsyncFeature -> UnsupportedEffectRowCode,
        TraitsFeature -> UnsupportedTraitConstraintCode,
        ObjectDispatchFeature -> UnsupportedObjectDispatchCode,
        CppImportsFeature -> UnsupportedCppImportCode,
        StagingFeature -> UnsupportedFeatureCode,
      ),
    )

  val all: List[CheckerProfile] =
    List(CosmocBasicExpr, Cosmo0Subset, MlttCore, MlttDependentPatterns)

  def byId(id: String): Option[CheckerProfile] =
    all.find(_.id == id)

  def firstUnsupportedFeatureForUnavailableProfile(
      profile: CheckerProfile,
  ): String =
    if profile.rejects(ObjectDispatchFeature) then ObjectDispatchFeature
    else if profile.rejects(EffectsFeature) then EffectsFeature
    else
      profile.rejectedFeatures.keys.toList.sorted.headOption
        .getOrElse("profile-implementation")

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
