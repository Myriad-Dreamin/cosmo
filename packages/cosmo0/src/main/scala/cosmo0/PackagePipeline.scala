package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.util.control.NonFatal

private[cosmo0] object PackagePipeline:
  def apply(compiler: Cosmo0): PackagePipeline =
    new PackagePipeline(compiler)

private[cosmo0] final class PackagePipeline(
    compiler: Cosmo0,
):
  private final case class PackageLoadData(
      rootPath: String,
      metadata: Cosmo0PackageMetadata,
      modules: List[Cosmo0PackageModule],
  )

  private final case class AnalyzedModule(
      pkgModule: Cosmo0PackageModule,
      untyped: UntypedModule,
  ):
    def key: String = moduleKey(pkgModule.modulePath)

    def localDeclarations: List[UntypedDecl] =
      untyped.declarations.filter {
        case _: UntypedImport             => false
        case _: UntypedCppNamespaceImport => false
        case _                            => true
      }

    def publicDeclarations: List[UntypedDecl] =
      localDeclarations.filter(_.visibility == UntypedVisibility.Public)

  private final case class ImportEdge(
      from: String,
      to: String,
      span: SourceSpan,
  )

  def load(rootPath: String): Result[Cosmo0Package] =
    val normalizedRoot = PackageNodePath.normalize(rootPath)
    val metadataPath = PackageNodePath.join(normalizedRoot, "cosmo.json")
    if !PackageNodeFs.existsSync(metadataPath) then
      return Result.failure(
        Phase.Check,
        List(
          diagnostic(
            "cosmo0.package.missing-metadata",
            s"cosmo0 package metadata not found at $metadataPath",
          ),
        ),
      )

    readMetadata(metadataPath) match
      case metadata if metadata.isFailure =>
        Result.failure(Phase.Check, metadata.diagnostics)
      case metadata if metadata.isUnsupported =>
        Result(
          Phase.Check,
          PhaseStatus.Unsupported,
          None,
          metadata.diagnostics,
        )
      case metadata =>
        val loaded = loadPackageData(normalizedRoot, metadata.value.get, Set.empty)
        if loaded.isFailure then return Result.failure(Phase.Check, loaded.diagnostics)

        val data = loaded.value.get
        val meta = data.metadata
        val modules = distinctModules(data.modules)
        if modules.isEmpty then
          Result.failure(
            Phase.Check,
            List(
              diagnostic(
                "cosmo0.package.no-sources",
                s"cosmo0 package ${meta.name} has no .cos sources under ${meta.sourceRoot}",
              ),
            ),
          )
        else
          Result.success(
            Phase.Check,
            Cosmo0Package(normalizedRoot, meta, modules.sortBy(module => moduleKey(module.modulePath))),
          )

  private def loadPackageData(
      rootPath: String,
      metadata: Cosmo0PackageMetadata,
      visiting: Set[String],
  ): Result[PackageLoadData] =
    if visiting.contains(rootPath) then
      return Result.failure(
        Phase.Check,
        List(
          diagnostic(
            "cosmo0.package.dependency-cycle",
            s"cosmo0 package dependency cycle includes $rootPath",
          ),
        ),
      )

    val dependencyModules = ListBuffer.empty[Cosmo0PackageModule]
    metadata.dependencies.foreach { dependency =>
      val dependencyRoot = PackageNodePath.normalize(PackageNodePath.join(rootPath, dependency))
      val dependencyMetadataPath = PackageNodePath.join(dependencyRoot, "cosmo.json")
      if !PackageNodeFs.existsSync(dependencyMetadataPath) then
        return Result.failure(
          Phase.Check,
          List(
            diagnostic(
              "cosmo0.package.missing-dependency",
              s"cosmo0 package dependency metadata not found at $dependencyMetadataPath",
            ),
          ),
        )

      readMetadata(dependencyMetadataPath) match
        case dependencyMetadata if dependencyMetadata.isFailure =>
          return Result.failure(Phase.Check, dependencyMetadata.diagnostics)
        case dependencyMetadata if dependencyMetadata.isUnsupported =>
          return Result(
            Phase.Check,
            PhaseStatus.Unsupported,
            None,
            dependencyMetadata.diagnostics,
          )
        case dependencyMetadata =>
          loadPackageData(dependencyRoot, dependencyMetadata.value.get, visiting + rootPath) match
            case loadedDependency if loadedDependency.isFailure =>
              return Result.failure(Phase.Check, loadedDependency.diagnostics)
            case loadedDependency if loadedDependency.isUnsupported =>
              return Result(
                Phase.Check,
                PhaseStatus.Unsupported,
                None,
                loadedDependency.diagnostics,
              )
            case loadedDependency =>
              dependencyModules ++= dependencyVisibleModules(loadedDependency.value.get.modules)
    }

    val ownModules = discoverSources(rootPath, metadata)
    Result.success(
      Phase.Check,
      PackageLoadData(rootPath, metadata, dependencyModules.toList ::: ownModules),
    )

  private def distinctModules(modules: List[Cosmo0PackageModule]): List[Cosmo0PackageModule] =
    val seen = mutable.LinkedHashSet.empty[String]
    modules.filter { module =>
      if seen.contains(module.source.name) then false
      else
        seen += module.source.name
        true
    }

  private def dependencyVisibleModules(modules: List[Cosmo0PackageModule]): List[Cosmo0PackageModule] =
    modules.filterNot(isPackageEntrypointModule)

  private def isPackageEntrypointModule(module: Cosmo0PackageModule): Boolean =
    module.modulePath == List("main")

  def check(pkg: Cosmo0Package): Result[CheckedPackage] =
    val elaborated = pkg.modules.map(module => module -> compiler.elaborate(module.source))
    val diagnostics = elaborated.flatMap(_._2.diagnostics)
    val failed = elaborated.exists(_._2.isFailure)
    val unsupported = elaborated.exists(_._2.isUnsupported)

    if failed then return Result.failure(Phase.Check, diagnostics)
    if unsupported then
      return Result(
        Phase.Check,
        PhaseStatus.Unsupported,
        None,
        diagnostics,
      )

    val analyzed = elaborated.map { case (module, result) =>
      AnalyzedModule(module, result.value.get)
    }

    val moduleDiagnostics = duplicateModuleDiagnostics(analyzed) :::
      duplicateDeclarationDiagnostics(analyzed)
    if moduleDiagnostics.nonEmpty then return Result.failure(Phase.Check, moduleDiagnostics)

    buildDependencyGraph(analyzed) match
      case Left(graphDiagnostics) =>
        Result.failure(Phase.Check, graphDiagnostics)
      case Right(edges) =>
        topologicalOrder(analyzed, edges) match
          case Left(cycleDiagnostics) =>
            Result.failure(Phase.Check, cycleDiagnostics)
          case Right(ordered) =>
            checkOrderedPackage(pkg, ordered)

  private def checkOrderedPackage(
      pkg: Cosmo0Package,
      ordered: List[AnalyzedModule],
  ): Result[CheckedPackage] =
    val stageDiagnostics =
      pkg.metadata.stageProfile.toList.flatMap(profileName => StageCapabilityRegistry.validate(profileName))
    if stageDiagnostics.nonEmpty then return Result.failure(Phase.Check, stageDiagnostics)

    val combinedSource = SourceFile(s"${pkg.metadata.outputModuleName}.cos", "")
    val declarations = ordered.flatMap(_.localDeclarations)
    val cIncludes = ordered.flatMap(_.untyped.cIncludes)
    val cppNamespaceImports = ordered.flatMap(_.untyped.cppNamespaceImports)
    val combinedModule =
      UntypedModule(
        combinedSource,
        declarations,
        combinedSource.span(0, 0),
        cIncludes,
        cppNamespaceImports,
      )

    SourceTyper().check(combinedModule) match
      case typed if typed.isFailure =>
        Result.failure(Phase.Check, typed.diagnostics)
      case typed =>
        val checkedModule = CheckedModule(typed.value.get)
        LirLowerer().lower(checkedModule.typed) match
          case lowered if lowered.isFailure =>
            Result.failure(Phase.Check, lowered.diagnostics)
          case lowered =>
            val loweredModule = LoweredModule(checkedModule, lowered.value.get)
            Result.success(
              Phase.Check,
              CheckedPackage(
                pkg.metadata,
                ordered.map(_.pkgModule),
                ordered.map(_.key),
                checkedModule,
                loweredModule,
              ),
            )

  private def readMetadata(metadataPath: String): Result[Cosmo0PackageMetadata] =
    val text =
      try PackageNodeFs.readFileSync(metadataPath, "utf8").asInstanceOf[String]
      catch
        case NonFatal(error) =>
          return Result.failure(
            Phase.Check,
            List(
              diagnostic(
                "cosmo0.package.metadata-read-failed",
                s"could not read cosmo0 package metadata at $metadataPath: ${messageOf(error)}",
              ),
            ),
          )

    val parsed =
      try js.JSON.parse(text).asInstanceOf[js.Dynamic]
      catch
        case NonFatal(error) =>
          return Result.failure(
            Phase.Check,
            List(
              diagnostic(
                "cosmo0.package.invalid-metadata",
                s"cosmo0 package metadata at $metadataPath is not valid JSON: ${messageOf(error)}",
              ),
            ),
          )

    val errors = ListBuffer.empty[Diagnostic]
    val name = requiredString(parsed, "name", metadataPath, errors)
    val version = requiredString(parsed, "version", metadataPath, errors)
    val sourceRoot = optionalString(parsed, "root").getOrElse("src")
    val target = optionalString(parsed, "target")
    val stageProfile = optionalString(parsed, "stageProfile")
    val sourceFiles = optionalStringList(parsed, "sources", metadataPath, errors)
    val dependencies = optionalStringList(parsed, "dependencies", metadataPath, errors).getOrElse(Nil)

    if errors.nonEmpty then
      Result.failure(Phase.Check, errors.toList)
    else if target.exists(_ != "cosmo0") then
      Result.unsupported(
        Phase.Check,
        diagnostic(
          "cosmo0.package.unsupported-target",
          s"cosmo0 package metadata target must be cosmo0 when present: ${target.get}",
        ),
      )
    else
      validateSourceRoot(sourceRoot) match
        case Some(rootDiagnostic) =>
          Result.unsupported(Phase.Check, rootDiagnostic)
        case None =>
          validateSourceFiles(sourceFiles) match
            case Some(sourceDiagnostic) =>
              Result.unsupported(Phase.Check, sourceDiagnostic)
            case None =>
              Result.success(
                Phase.Check,
                Cosmo0PackageMetadata(
                  name.get,
                  version.get,
                  sourceRoot,
                  target,
                  stageProfile,
                  sourceFiles,
                  dependencies,
                ),
              )

  private def discoverSources(
      rootPath: String,
      metadata: Cosmo0PackageMetadata,
  ): List[Cosmo0PackageModule] =
    val sourceRootPath = PackageNodePath.join(rootPath, metadata.sourceRoot)
    if !PackageNodeFs.existsSync(sourceRootPath) then Nil
    else
      metadata.sourceFiles match
        case Some(files) =>
          files.flatMap(relativeSourcePath =>
            loadListedSource(rootPath, metadata.sourceRoot, relativeSourcePath),
          )
        case None =>
          scanSourceDir(rootPath, metadata.sourceRoot, "")

  private def loadListedSource(
      packageRoot: String,
      sourceRoot: String,
      relativeSourcePath: String,
  ): Option[Cosmo0PackageModule] =
    val absolutePath = PackageNodePath.join(packageRoot, sourceRoot, relativeSourcePath)
    val stat =
      try Some(PackageNodeFs.statSync(absolutePath))
      catch
        case NonFatal(_) => None

    stat.filter(_.isFile()).map { _ =>
      val text = PackageNodeFs.readFileSync(absolutePath, "utf8").asInstanceOf[String]
      val sourceName = PackageNodePath.join(packageRoot, sourceRoot, relativeSourcePath)
      Cosmo0PackageModule(
        modulePath(relativeSourcePath),
        SourceFile(sourceName, text),
      )
    }

  private def scanSourceDir(
      packageRoot: String,
      sourceRoot: String,
      relativeDir: String,
  ): List[Cosmo0PackageModule] =
    val absoluteDir =
      if relativeDir.isEmpty then PackageNodePath.join(packageRoot, sourceRoot)
      else PackageNodePath.join(packageRoot, sourceRoot, relativeDir)
    val entries =
      try PackageNodeFs.readdirSync(absoluteDir).toList.map(_.toString).sorted
      catch
        case NonFatal(_) => return Nil

    entries.flatMap { entry =>
      val relativePath = joinRelative(relativeDir, entry)
      val absolutePath = PackageNodePath.join(packageRoot, sourceRoot, relativePath)
      val stat =
        try Some(PackageNodeFs.statSync(absolutePath))
        catch
          case NonFatal(_) => None

      stat.toList.flatMap { info =>
        if info.isDirectory() then scanSourceDir(packageRoot, sourceRoot, relativePath)
        else if info.isFile() && entry.endsWith(".cos") then
          val text = PackageNodeFs.readFileSync(absolutePath, "utf8").asInstanceOf[String]
          val sourceName = PackageNodePath.join(packageRoot, sourceRoot, relativePath)
          List(
            Cosmo0PackageModule(
              modulePath(relativePath),
              SourceFile(sourceName, text),
            ),
          )
        else Nil
      }
    }

  private def duplicateModuleDiagnostics(
      modules: List[AnalyzedModule],
  ): List[Diagnostic] =
    modules
      .groupBy(_.key)
      .toList
      .sortBy(_._1)
      .collect {
        case (key, values) if values.length > 1 =>
          diagnostic(
            "cosmo0.package.duplicate-module",
            s"cosmo0 package contains duplicate module path $key",
            Some(values.head.untyped.span),
          )
      }

  private def duplicateDeclarationDiagnostics(
      modules: List[AnalyzedModule],
  ): List[Diagnostic] =
    modules
      .flatMap(module =>
        module.localDeclarations.map(declaration =>
          declaration.name -> (module.key, declaration.span),
        ),
      )
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .collect {
        case (name, declarations) if declarations.length > 1 =>
          val moduleNames = declarations.map(_._2._1).distinct.sorted.mkString(", ")
          diagnostic(
            "cosmo0.package.duplicate-declaration",
            s"cosmo0 package declares $name in multiple modules: $moduleNames",
            Some(declarations.head._2._2),
          )
      }

  private def buildDependencyGraph(
      modules: List[AnalyzedModule],
  ): Either[List[Diagnostic], Map[String, List[ImportEdge]]] =
    val byKey = modules.map(module => module.key -> module).toMap
    val diagnostics = ListBuffer.empty[Diagnostic]
    val edges = mutable.LinkedHashMap.empty[String, ListBuffer[ImportEdge]]
    modules.sortBy(_.key).foreach { module =>
      val moduleEdges = edges.getOrElseUpdate(module.key, ListBuffer.empty)
      module.untyped.declarations.foreach {
        case importDecl: UntypedImport =>
          val importedKey = moduleKey(importDecl.path.parts)
          byKey.get(importedKey) match
            case None =>
              diagnostics += diagnostic(
                "cosmo0.package.missing-import",
                s"module ${module.key} imports missing module $importedKey",
                Some(importDecl.path.span),
              )
            case Some(importedModule) =>
              importDecl.dest.foreach { dest =>
                val importedName = dest.parts.lastOption.getOrElse(dest.text)
                val publicNames = importedModule.publicDeclarations.map(_.name).toSet
                if !publicNames.contains(importedName) then
                  diagnostics += diagnostic(
                    "cosmo0.package.missing-import-member",
                    s"module ${module.key} imports $importedName from $importedKey, but that declaration is not public in the target module",
                    Some(dest.span),
                  )
              }
              moduleEdges += ImportEdge(module.key, importedKey, importDecl.span)
        case _: UntypedCppNamespaceImport =>
        case _ =>
      }
      }

    if diagnostics.nonEmpty then Left(diagnostics.toList)
    else
      Right(
        edges.toList
          .sortBy(_._1)
          .map { case (key, value) => key -> value.toList.sortBy(_.to) }
          .toMap,
      )

  private def topologicalOrder(
      modules: List[AnalyzedModule],
      edges: Map[String, List[ImportEdge]],
  ): Either[List[Diagnostic], List[AnalyzedModule]] =
    val byKey = modules.map(module => module.key -> module).toMap
    val permanent = mutable.LinkedHashSet.empty[String]
    val temporary = mutable.LinkedHashSet.empty[String]
    val stack = ListBuffer.empty[String]
    val ordered = ListBuffer.empty[String]
    val diagnostics = ListBuffer.empty[Diagnostic]

    def visit(key: String): Unit =
      if permanent.contains(key) || diagnostics.nonEmpty then ()
      else if temporary.contains(key) then
        val cycleStart = stack.indexOf(key)
        val cycle =
          if cycleStart >= 0 then stack.drop(cycleStart).toList :+ key
          else List(key, key)
        val span = edges.getOrElse(key, Nil).headOption.map(_.span)
        diagnostics += diagnostic(
          "cosmo0.package.import-cycle",
          s"cosmo0 package import cycle is unsupported: ${cycle.mkString(" -> ")}",
          span,
        )
      else
        temporary += key
        stack += key
        edges.getOrElse(key, Nil).foreach(edge => visit(edge.to))
        stack.remove(stack.length - 1)
        temporary -= key
        permanent += key
        ordered += key

    modules.sortBy(_.key).foreach(module => visit(module.key))

    if diagnostics.nonEmpty then Left(diagnostics.toList)
    else Right(ordered.toList.flatMap(byKey.get))

  private def requiredString(
      parsed: js.Dynamic,
      field: String,
      metadataPath: String,
      errors: ListBuffer[Diagnostic],
  ): Option[String] =
    optionalString(parsed, field).filter(_.nonEmpty) match
      case Some(value) => Some(value)
      case None =>
        errors += diagnostic(
          "cosmo0.package.missing-metadata-field",
          s"cosmo0 package metadata at $metadataPath is missing required string field $field",
        )
        None

  private def optionalString(parsed: js.Dynamic, field: String): Option[String] =
    val value = parsed.selectDynamic(field)
    if js.isUndefined(value) || value == null then None
    else if js.typeOf(value) == "string" then Some(value.asInstanceOf[String])
    else None

  private def optionalStringList(
      parsed: js.Dynamic,
      field: String,
      metadataPath: String,
      errors: ListBuffer[Diagnostic],
  ): Option[List[String]] =
    val value = parsed.selectDynamic(field)
    if js.isUndefined(value) || value == null then None
    else if js.Array.isArray(value.asInstanceOf[js.Any]) then
      val array = value.asInstanceOf[js.Array[js.Any]]
      val values = array.toList
      if values.forall(item => js.typeOf(item) == "string") then
        Some(values.map(_.asInstanceOf[String]))
      else
        errors += diagnostic(
          "cosmo0.package.invalid-metadata-field",
          s"cosmo0 package metadata at $metadataPath field $field must be an array of strings",
        )
        None
    else
      errors += diagnostic(
        "cosmo0.package.invalid-metadata-field",
        s"cosmo0 package metadata at $metadataPath field $field must be an array of strings",
      )
      None

  private def validateSourceRoot(root: String): Option[Diagnostic] =
    val parts = root.split("[/\\\\]").toList.filter(_.nonEmpty)
    if root.trim.isEmpty then
      Some(
        diagnostic(
          "cosmo0.package.unsupported-root",
          "cosmo0 package root must be a non-empty relative path",
        ),
      )
    else if root.startsWith("/") || root.startsWith("\\") then
      Some(
        diagnostic(
          "cosmo0.package.unsupported-root",
          s"cosmo0 package root must be relative: $root",
        ),
      )
    else if parts.exists(part => part == "." || part == "..") then
      Some(
        diagnostic(
          "cosmo0.package.unsupported-root",
          s"cosmo0 package root must stay inside the package: $root",
        ),
      )
    else None

  private def validateSourceFiles(sourceFiles: Option[List[String]]): Option[Diagnostic] =
    sourceFiles match
      case Some(Nil) =>
        Some(
          diagnostic(
            "cosmo0.package.unsupported-sources",
            "cosmo0 package sources must name at least one .cos source when present",
          ),
        )
      case Some(files) =>
        files.find(invalidSourceFile).map { source =>
          diagnostic(
            "cosmo0.package.unsupported-sources",
            s"cosmo0 package source entries must be relative .cos files inside the source root: $source",
          )
        }
      case None =>
        None

  private def invalidSourceFile(source: String): Boolean =
    val parts = source.split("[/\\\\]").toList.filter(_.nonEmpty)
    source.trim.isEmpty ||
      source.startsWith("/") ||
      source.startsWith("\\") ||
      !source.endsWith(".cos") ||
      parts.exists(part => part == "." || part == "..")

  private def modulePath(relativeSourcePath: String): List[String] =
    relativeSourcePath
      .stripSuffix(".cos")
      .split("[/\\\\]")
      .toList
      .filter(_.nonEmpty)

  private def moduleKey(parts: List[String]): String =
    parts.mkString("/")

  private def joinRelative(left: String, right: String): String =
    if left.isEmpty then right else s"$left/$right"

  private def diagnostic(
      code: String,
      message: String,
      span: Option[SourceSpan] = None,
  ): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      code,
      message,
      span,
    )

  private def messageOf(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getName)

@js.native
@JSImport("fs",JSImport.Namespace)
private object PackageNodeFs extends js.Object:
  def existsSync(path: String): Boolean = js.native
  def readFileSync(path: String, encoding: String): js.Any = js.native
  def readdirSync(path: String): js.Array[String] = js.native
  def statSync(path: String): PackageNodeStats = js.native

@js.native
private trait PackageNodeStats extends js.Object:
  def isDirectory(): Boolean = js.native
  def isFile(): Boolean = js.native

@js.native
@JSImport("path",JSImport.Namespace)
private object PackageNodePath extends js.Object:
  def join(paths: String*): String = js.native
  def normalize(path: String): String = js.native
