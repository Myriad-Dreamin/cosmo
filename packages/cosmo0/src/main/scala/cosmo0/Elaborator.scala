package cosmo0

import scala.collection.mutable.ListBuffer

import cosmo.syntax
import cosmo.syntax.*

object UntypedElaborator:
  val defaultStandardGenericNames: Set[String] =
    Set("Arena", "Box", "Id", "Map", "Option", "Ptr", "Ref", "RefMut", "Result", "Set", "Vec")

  def apply(): UntypedElaborator =
    new UntypedElaborator(defaultStandardGenericNames)

final class UntypedElaborator(
    standardGenericNames: Set[String],
):
  def elaborate(parsed: ParsedModule): Result[UntypedModule] =
    val state = State(parsed.source, standardGenericNames)
    val declarations = parsed.ast.stmts.flatMap(state.moduleDecl)
    val diagnostics = state.diagnostics.toList

    if diagnostics.isEmpty then
      Result.success(
        Phase.Check,
        UntypedModule(
          parsed.source,
          declarations,
          state.nodeSpan(parsed.ast),
          state.cIncludes.toList,
          state.cppNamespaceImports.toList,
        ),
      )
    else
      Result(
        Phase.Check,
        PhaseStatus.Unsupported,
        None,
        diagnostics,
      )

  private final class State(
      source: SourceFile,
      standardGenericNames: Set[String],
  ):
    val diagnostics: ListBuffer[Diagnostic] = ListBuffer.empty
    val cIncludes: ListBuffer[SourceCInclude] = ListBuffer.empty
    val cppNamespaceImports: ListBuffer[SourceCppNamespaceImport] = ListBuffer.empty

    private val assignmentOps =
      Set("=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=")

    private val higherOrderMethodNames =
      Set("filter", "flatMap", "fold", "forEach", "foreach", "map")

    def moduleDecl(node: syntax.Node): Option[UntypedDecl] =
      unwrapSemi(node) match
        case None => None
        case Some(importNode: Import) => importDecl(importNode)
        case Some(classNode: Class)   => classDecl(classNode)
        case Some(defNode: Def)       => functionDecl(defNode)
        case Some(valueNode: Val)     => valueDecl(valueNode, UntypedValueKind.Val)
        case Some(valueNode: Var)     => valueDecl(valueNode, UntypedValueKind.Var)
        case Some(typeNode: Typ)      => typeAlias(typeNode)
        case Some(implNode: Impl)     => implDecl(implNode)
        case Some(decorated: Decorate) => decoratedModuleDecl(decorated)
        case Some(caseNode: Case) =>
          unsupported(
            caseNode,
            "cosmo0.elaborate.unsupported.top-level-case",
            "case variants are only supported inside cosmo0 classes",
          )
        case Some(other) =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.top-level",
            s"${constructName(other)} is not a supported top-level cosmo0 declaration",
          )

    private final case class ExternDecoratorArgs(
        name: Option[String] = None,
        supportLibrary: Option[String] = None,
    )

    private final case class IncludeDecoratorArgs(
        path: String,
        kind: Option[String] = None,
    )

    private def decoratedModuleDecl(node: Decorate): Option[UntypedDecl] =
      includeDecorator(node.lhs) match
        case Some(include) =>
          unwrapSemi(node.rhs) match
            case None =>
              cIncludes += include
              None
            case Some(other) =>
              unsupported(
                other,
                "cosmo0.elaborate.invalid-include",
                "@include(...) must be a file-level decorator terminated by a semicolon",
              )
        case None if isIncludeDecorator(node.lhs) =>
          None
        case None =>
          externDecorator(node.lhs).flatMap { binding =>
            unwrapSemi(node.rhs) match
              case Some(defNode: Def) =>
                functionDecl(defNode, Some(binding))
              case Some(other) =>
                unsupported(
                  other,
                  "cosmo0.elaborate.invalid-extern",
                  "@extern(\"c\") can only decorate top-level function declarations",
                )
              case None =>
                unsupported(
                  node,
                  "cosmo0.elaborate.invalid-extern",
                  "@extern(\"c\") must decorate a top-level function declaration",
                )
          }

    private def includeDecorator(node: syntax.Node): Option[SourceCInclude] =
      node match
        case Apply(Ident("include"), args, false) =>
          includeDecoratorArgs(node, args).flatMap { values =>
            validateIncludeKind(values, node).flatMap {
              case "c" =>
                cIncludeHeader(values.path, node).map(header => SourceCInclude(header, nodeSpan(node)))
              case kind =>
                unsupported(
                  node,
                  "cosmo0.elaborate.unsupported.include-kind",
                  s"include kind $kind is not supported by cosmo0",
                )
            }
          }
        case Ident("include") =>
          unsupported(
            node,
            "cosmo0.elaborate.invalid-include",
            "@include(...) expects an include path string",
          )
        case Ident("include-c") | Apply(Ident("include-c"), _, false) =>
          unsupported(
            node,
            "cosmo0.elaborate.invalid-include",
            "use @include(\"header.h\", kind = \"c\") instead of @include-c(...)",
          )
        case _ => None

    private def isIncludeDecorator(node: syntax.Node): Boolean =
      node match
        case Ident("include") | Apply(Ident("include"), _, false) => true
        case Ident("include-c") | Apply(Ident("include-c"), _, false) => true
        case _ => false

    private def includeDecoratorArgs(
        node: syntax.Node,
        args: List[syntax.Node],
    ): Option[IncludeDecoratorArgs] =
      args match
        case StrLit(path) :: rest =>
          var kind: Option[String] = None
          var ok = true

          rest.foreach {
            case arg @ KeyedArg(Ident("kind"), StrLit(value)) =>
              if kind.nonEmpty then
                report(
                  arg,
                  "cosmo0.elaborate.invalid-include",
                  "include decorator repeats argument kind",
                )
                ok = false
              else kind = Some(value)
            case arg @ KeyedArg(Ident("kind"), _) =>
              report(
                arg,
                "cosmo0.elaborate.invalid-include",
                "include decorator argument kind must be a string literal",
              )
              ok = false
            case arg @ KeyedArg(Ident(key), _) =>
              report(
                arg,
                "cosmo0.elaborate.invalid-include",
                s"include decorator does not support argument $key",
              )
              ok = false
            case arg @ KeyedArg(_, _) =>
              report(
                arg,
                "cosmo0.elaborate.invalid-include",
                "include decorator argument names must be identifiers",
              )
              ok = false
            case other =>
              report(
                other,
                "cosmo0.elaborate.invalid-include",
                "include decorator arguments after the path must be keyed arguments",
              )
              ok = false
          }

          if ok then Some(IncludeDecoratorArgs(path, kind)) else None
        case Nil =>
          unsupported(
            node,
            "cosmo0.elaborate.invalid-include",
            "@include(...) expects an include path string",
          )
        case other :: _ =>
          unsupported(
            other,
            "cosmo0.elaborate.invalid-include",
            "include path must be a string literal",
          )

    private def externDecorator(node: syntax.Node): Option[SourceExternBinding] =
      node match
        case Apply(Ident("extern"), args, false) =>
          externDecoratorArgs(node, args)
        case Ident("extern") =>
          unsupported(
            node,
            "cosmo0.elaborate.invalid-extern",
            "extern decorators must specify an ABI, for example @extern(\"c\")",
          )
        case _ =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.decorator",
            "only @extern(\"c\") decorators are supported on top-level cosmo0 function declarations",
          )

    private def externDecoratorArgs(
        node: syntax.Node,
        args: List[syntax.Node],
    ): Option[SourceExternBinding] =
      args match
        case StrLit(abi) :: rest if abi == TrustedExternAbi.directCAbiName =>
          collectExternDecoratorArgs(rest).flatMap { values =>
            if validateExternDecoratorArgs(values, node) then
              Some(
                SourceExternBinding(
                  abi,
                  values.name,
                  values.supportLibrary,
                  nodeSpan(node),
                ),
              )
            else None
          }
        case StrLit(abi) :: _ =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.extern-abi",
            s"extern ABI $abi is not supported by cosmo0",
          )
        case Nil =>
          unsupported(
            node,
            "cosmo0.elaborate.invalid-extern",
            "extern decorators must specify an ABI string",
          )
        case other :: _ =>
          unsupported(
            other,
            "cosmo0.elaborate.invalid-extern",
            "extern decorator ABI must be a string literal",
          )

    private def collectExternDecoratorArgs(args: List[syntax.Node]): Option[ExternDecoratorArgs] =
      var values = ExternDecoratorArgs()
      var ok = true

      def setOnce(
          node: syntax.Node,
          key: String,
          previous: Option[String],
          update: String => ExternDecoratorArgs,
          value: String,
      ): Unit =
        if previous.nonEmpty then
          report(
            node,
            "cosmo0.elaborate.invalid-extern",
            s"extern decorator repeats argument $key",
          )
          ok = false
        else values = update(value)

      args.foreach {
        case arg @ KeyedArg(Ident("name"), StrLit(value)) =>
          setOnce(
            arg,
            "name",
            values.name,
            value => values.copy(name = Some(value)),
            value,
          )
        case arg @ KeyedArg(Ident("symbol"), StrLit(value)) =>
          setOnce(
            arg,
            "symbol",
            values.name,
            value => values.copy(name = Some(value)),
            value,
          )
        case arg @ KeyedArg(Ident("supportLibrary"), StrLit(value)) =>
          setOnce(
            arg,
            "supportLibrary",
            values.supportLibrary,
            value => values.copy(supportLibrary = Some(value)),
            value,
          )
        case arg @ KeyedArg(Ident("include"), _) =>
          report(
            arg,
            "cosmo0.elaborate.invalid-extern",
            "extern decorators do not accept include; use a file-level @include(\"header.h\", kind = \"c\") directive",
          )
          ok = false
        case arg @ KeyedArg(Ident(key), _) if Set("name", "symbol", "supportLibrary").contains(key) =>
          report(
            arg,
            "cosmo0.elaborate.invalid-extern",
            s"extern decorator argument $key must be a string literal",
          )
          ok = false
        case arg @ KeyedArg(Ident(key), _) =>
          report(
            arg,
            "cosmo0.elaborate.invalid-extern",
            s"extern decorator does not support argument $key",
          )
          ok = false
        case arg @ KeyedArg(_, _) =>
          report(
            arg,
            "cosmo0.elaborate.invalid-extern",
            "extern decorator argument names must be identifiers",
          )
          ok = false
        case other =>
          report(
            other,
            "cosmo0.elaborate.invalid-extern",
            "extern decorator arguments after the ABI must be keyed string arguments",
          )
          ok = false
      }

      if ok then Some(values) else None

    private def validateExternDecoratorArgs(
        values: ExternDecoratorArgs,
        node: syntax.Node,
    ): Boolean =
      val nameOk = values.name.forall { value =>
        val ok = CppQualifiedSymbol.isIdentifier(value)
        if !ok then
          report(
            node,
            "cosmo0.elaborate.invalid-extern",
            s"extern C name $value is not an unqualified C identifier",
          )
        ok
      }
      val supportLibraryOk = values.supportLibrary.forall { value =>
        SupportLibraryId.validate(value) match
          case Some(message) =>
            report(
              node,
              "cosmo0.elaborate.invalid-extern",
              s"extern supportLibrary $message",
            )
            false
          case None =>
            true
      }
      nameOk && supportLibraryOk

    private def validateIncludeKind(
        values: IncludeDecoratorArgs,
        node: syntax.Node,
    ): Option[String] =
      if !isRequirementValue(values.path) then
        unsupported(
          node,
          "cosmo0.elaborate.invalid-include",
          "include path must be a non-empty single-line string",
        )
      else
        val kind = values.kind.orElse(inferIncludeKind(values.path))
        kind match
          case Some(value) if isRequirementValue(value) => Some(value)
          case Some(_) =>
            unsupported(
              node,
              "cosmo0.elaborate.invalid-include",
              "include kind must be a non-empty single-line string",
            )
          case None =>
            unsupported(
              node,
              "cosmo0.elaborate.invalid-include",
              "include kind could not be inferred from the path extension; specify kind = \"c\"",
            )

    private def inferIncludeKind(path: String): Option[String] =
      val normalized = includePathForExtension(path).toLowerCase
      if normalized.endsWith(".h") then Some("c") else None

    private def includePathForExtension(path: String): String =
      if isIncludeSpecifier(path) then path.substring(1, path.length - 1)
      else path

    private def cIncludeHeader(path: String, node: syntax.Node): Option[String] =
      if isIncludeSpecifier(path) then Some(path)
      else if isRequirementValue(path) then Some(s"<$path>")
      else
        unsupported(
          node,
          "cosmo0.elaborate.invalid-include",
          "C include path must be a non-empty single-line string",
        )

    private def isIncludeSpecifier(value: String): Boolean =
      isRequirementValue(value) &&
        ((value.startsWith("<") && value.endsWith(">") && value.length > 2) ||
          (value.startsWith("\"") && value.endsWith("\"") && value.length > 2))

    private def isRequirementValue(value: String): Boolean =
      value.nonEmpty && !value.exists(ch => ch == '\n' || ch == '\r')

    private def importDecl(node: Import): Option[UntypedDecl] =
      node.path match
        case StrLit(header) if header.startsWith(SourceCppNamespaceImport.headerPrefix) =>
          cppImportDecl(node)
        case _ =>
          cosmoImportDecl(node)

    private def cppImportDecl(node: Import): Option[UntypedDecl] =
      node.path match
        case StrLit(header) if SourceCppNamespaceImport.isCppHeader(header) =>
          node.dest match
            case None =>
              unsupported(
                node,
                "cosmo1.name.unsupported-cpp-header-import",
                s"C++ header import $header must bind an explicit namespace alias",
              )
            case Some(As(namespaceNode, Ident(alias))) =>
              cppNamespacePath(namespaceNode, Some(nodeSpan(node))).flatMap { namespace =>
                if !CppQualifiedSymbol.isIdentifier(alias) then
                  unsupported(
                    namespaceNode,
                    "cosmo1.name.invalid-cpp-namespace-import",
                    s"C++ namespace alias $alias is not a valid identifier",
                  )
                else
                  val importValue = SourceCppNamespaceImport(namespace, alias, List(header), nodeSpan(node))
                  cppNamespaceImports += importValue
                  Some(UntypedCppNamespaceImport(importValue, declarationVisibility(node)))
              }
            case Some(_) =>
              unsupported(
                node,
                "cosmo1.name.invalid-cpp-namespace-import",
                s"C++ import $header must use import <namespace> as <alias> from \"$header\"",
              )
        case StrLit(header) if header.startsWith(SourceCppNamespaceImport.headerPrefix) =>
          unsupported(
            node,
            "cosmo1.name.invalid-cpp-namespace-import",
            s"C++ header import $header must name a header after ${SourceCppNamespaceImport.headerPrefix}",
          )
        case _ => None

    private def cppNamespacePath(
        node: syntax.Node,
        fallbackSpan: Option[SourceSpan],
    ): Option[CppQualifiedSymbol] =
      pathFromNode(node, fallbackSpan).flatMap { path =>
        if path.parts.forall(CppQualifiedSymbol.isIdentifier) then
          Some(CppQualifiedSymbol(path.parts, absolute = true))
        else
          unsupported(
            node,
            "cosmo1.name.invalid-cpp-namespace-import",
            s"C++ namespace ${path.text} is not a valid qualified namespace path",
          )
      }

    private def cosmoImportDecl(node: Import): Option[UntypedImport] =
      val span = nodeSpan(node)
      val path = pathFromNode(node.path, Some(span))
      val dest = node.dest.fold[Option[Option[UntypedPath]]](Some(None)) { destNode =>
        pathFromNode(destNode, Some(span)).map(Some(_))
      }
      path.zip(dest).map { case (p, d) =>
        UntypedImport(p, d, span, declarationVisibility(node))
      }

    private def classDecl(node: Class): Option[UntypedDecl] =
      if node.ab then traitDecl(node)
      else concreteClassDecl(node)

    private def concreteClassDecl(node: Class): Option[UntypedClass] =
      if hasExplicitTypeParams(node.ps) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.generic-class",
          "user-defined generic classes are outside the initial cosmo0 subset",
        )
      else if node.ps.exists(_.nonEmpty) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.class-params",
          "class parameter lists are outside the initial cosmo0 subset",
        )
      else
        val members = classMembers(node.body)
        sequence(members).map(UntypedClass(node.name.name, _, nodeSpan(node), declarationVisibility(node)))

    private def traitDecl(node: Class): Option[UntypedTrait] =
      if hasExplicitTypeParams(node.ps) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.generic-trait",
          "generic traits are outside the initial cosmo0 subset",
        )
      else if node.ps.exists(_.nonEmpty) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.trait-params",
          "trait parameter lists are outside the initial cosmo0 subset",
        )
      else
        val members = traitMembers(node.body)
        sequence(members).map(UntypedTrait(node.name.name, _, nodeSpan(node), declarationVisibility(node)))

    private def traitMembers(node: syntax.Node): List[Option[UntypedFunction]] =
      unwrapSemi(node) match
        case None => Nil
        case Some(block: Block) =>
          block.stmts.map(traitMember)
        case Some(other) =>
          List(
            unsupported(
              other,
              "cosmo0.elaborate.unsupported.trait-body",
              "cosmo0 trait bodies must be blocks containing method signatures",
            ),
          )

    private def traitMember(node: syntax.Node): Option[UntypedFunction] =
      unwrapSemi(node) match
        case None => None
        case Some(defNode: Def) =>
          functionDecl(defNode).flatMap { fn =>
            if fn.body.nonEmpty then
              unsupported(
                defNode,
                "cosmo0.elaborate.unsupported.trait-method-body",
                "cosmo0 trait methods cannot define bodies",
              )
            else Some(fn)
          }
        case Some(other) =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.trait-member",
            s"${constructName(other)} is not a supported cosmo0 trait member",
          )

    private def classMembers(node: syntax.Node): List[Option[UntypedClassMember]] =
      unwrapSemi(node) match
        case None => Nil
        case Some(block: Block) =>
          block.stmts.map(classMember)
        case Some(caseBlock: CaseBlock) =>
          caseBlock.stmts.map(variantDecl)
        case Some(other) =>
          List(
            unsupported(
              other,
              "cosmo0.elaborate.unsupported.class-body",
              "cosmo0 class bodies must be blocks containing fields, methods, aliases, or case variants",
            ),
          )

    private def classMember(node: syntax.Node): Option[UntypedClassMember] =
      unwrapSemi(node) match
        case None => None
        case Some(valueNode: Val) => valueDecl(valueNode, UntypedValueKind.Val)
        case Some(valueNode: Var) => valueDecl(valueNode, UntypedValueKind.Var)
        case Some(defNode: Def)   => functionDecl(defNode)
        case Some(typeNode: Typ)  => typeAlias(typeNode)
        case Some(caseNode: Case) => variantDecl(caseNode)
        case Some(decorated: Decorate) =>
          unsupported(
            decorated,
            "cosmo0.elaborate.unsupported.decorator",
            "decorators and staging annotations are outside the initial cosmo0 subset",
          )
        case Some(implNode: Impl) =>
          unsupported(
            implNode,
            "cosmo0.elaborate.unsupported.impl",
            "impl declarations are outside the initial cosmo0 subset",
          )
        case Some(other) =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.class-member",
            s"${constructName(other)} is not a supported cosmo0 class member",
          )

    private def functionDecl(
        node: Def,
        externBinding: Option[SourceExternBinding] = None,
    ): Option[UntypedFunction] =
      if hasExplicitTypeParams(node.params) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.generic-function",
          "user-defined generic functions are outside the initial cosmo0 subset",
        )
      else if externBinding.nonEmpty && node.rhs.nonEmpty then
        unsupported(
          node,
          "cosmo0.elaborate.invalid-extern",
          "extern function declarations cannot define a cosmo0 body",
        )
      else
        val span = nodeSpan(node)
        val params = sequence(node.params.getOrElse(Nil).map(param))
        val returnType = node.ret.fold[Option[Option[UntypedType]]](Some(None)) { ret =>
          typeFromNode(ret, Some(span)).map(Some(_))
        }
        val body = node.rhs.fold[Option[Option[UntypedExpr]]](Some(None)) { rhs =>
          expr(rhs).map(Some(_))
        }
        params.zip(returnType).zip(body).map { case ((ps, rt), b) =>
          UntypedFunction(node.name.name, ps, rt, b, span, externBinding, declarationVisibility(node))
        }

    private def valueDecl(
        node: Val,
        kind: UntypedValueKind,
    ): Option[UntypedValueDecl] =
      val span = nodeSpan(node)
      val valueType = node.ty.fold[Option[Option[UntypedType]]](Some(None)) { ty =>
        typeFromNode(ty, Some(span)).map(Some(_))
      }
      val init = node.init.fold[Option[Option[UntypedExpr]]](Some(None)) { value =>
        expr(value).map(Some(_))
      }
      valueType.zip(init).map { case (t, i) =>
        UntypedValueDecl(kind, node.name.name, t, i, span, declarationVisibility(node))
      }

    private def valueDecl(
        node: Var,
        kind: UntypedValueKind,
    ): Option[UntypedValueDecl] =
      val span = nodeSpan(node)
      val valueType = node.ty.fold[Option[Option[UntypedType]]](Some(None)) { ty =>
        typeFromNode(ty, Some(span)).map(Some(_))
      }
      val init = node.init.fold[Option[Option[UntypedExpr]]](Some(None)) { value =>
        expr(value).map(Some(_))
      }
      valueType.zip(init).map { case (t, i) =>
        UntypedValueDecl(kind, node.name.name, t, i, span, declarationVisibility(node))
      }

    private def typeAlias(node: Typ): Option[UntypedTypeAlias] =
      node.init.orElse(node.ty) match
        case Some(targetNode) =>
          typeFromNode(targetNode, Some(nodeSpan(node))).map(target =>
            UntypedTypeAlias(node.name.name, target, nodeSpan(node), declarationVisibility(node)),
          )
        case None =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.type-alias-target",
            "cosmo0 type aliases must name a concrete target type",
          )

    private def implDecl(node: Impl): Option[UntypedImpl] =
      if node.params.exists(_.nonEmpty) then
        return unsupported(
          node,
          "cosmo0.elaborate.unsupported.impl",
          "generic impl declarations are outside the initial cosmo0 subset",
        )

      val traitPath = node.lhs match
        case Some(traitNode) =>
          pathFromNode(traitNode, Some(nodeSpan(traitNode)))
        case None =>
          return unsupported(
            node,
            "cosmo0.elaborate.unsupported.impl",
            "cosmo0 impl declarations must name a trait and target type",
          )

      val targetPath = pathFromNode(node.rhs, Some(nodeSpan(node.rhs)))
      if targetPath.exists(_.parts.length != 1) then
        return unsupported(
          node.rhs,
          "cosmo0.elaborate.unsupported.impl",
          "impl targets must be local concrete types",
        )

      val members = sequence(classMembers(node.body))
      traitPath.zip(targetPath).zip(members).flatMap { case ((traitName, target), implMembers) =>
        validateTraitImplMembers(node, implMembers).map(_ =>
          UntypedImpl(traitName, target, implMembers, nodeSpan(node), UntypedVisibility.Private),
        )
      }

    private def validateTraitImplMembers(
        node: Impl,
        members: List[UntypedClassMember],
    ): Option[Unit] =
      val functions = members.collect { case fn: UntypedFunction => fn }
      if functions.length != members.length then
        return unsupported(
          node,
          "cosmo0.elaborate.unsupported.impl",
          "trait impl declarations may only contain methods",
        )
      functions.find(_.body.isEmpty) match
        case Some(_) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.impl",
            "trait impl methods must define bodies",
          )
        case None =>
          Some(())

    private def variantDecl(node: Case): Option[UntypedVariant] =
      if node.body.nonEmpty then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.variant-body",
          "enum-style cosmo0 case variants cannot define branch bodies",
        )
      else
        node.cond match
          case name: Ident =>
            Some(UntypedVariant(name.name, Nil, nodeSpan(node)))
          case Apply(name: Ident, args, false) =>
            val fields = args.map(variantField)
            sequence(fields).map(UntypedVariant(name.name, _, nodeSpan(node)))
          case other =>
            unsupported(
              other,
              "cosmo0.elaborate.unsupported.variant",
              "cosmo0 case variants must be a variant name with optional payload types",
            )

    private def variantField(node: syntax.Node): Option[UntypedVariantField] =
      node match
        case KeyedArg(name: Ident, valueType) =>
          typeFromNode(valueType, Some(nodeSpan(node))).map(t =>
            UntypedVariantField(Some(name.name), t, nodeSpan(node)),
          )
        case KeyedArg(_, _) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.variant-field",
            "named variant fields must use an identifier field name",
          )
        case other =>
          typeFromNode(other, Some(nodeSpan(other))).map(t =>
            UntypedVariantField(None, t, nodeSpan(other)),
          )

    private def param(node: Param): Option[UntypedParam] =
      if node.ct then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.type-param",
          "explicit compile-time parameters are outside the initial cosmo0 subset",
        )
      else
        val span = nodeSpan(node)
        val valueType = node.ty.fold[Option[Option[UntypedType]]](Some(None)) { ty =>
          typeFromNode(ty, Some(span)).map(Some(_))
        }
        val default = node.init.fold[Option[Option[UntypedExpr]]](Some(None)) { value =>
          expr(value).map(Some(_))
        }
        valueType.zip(default).map { case (t, d) =>
          UntypedParam(node.name.name, t, d, span)
        }

    private def blockItem(node: syntax.Node): Option[UntypedBlockItem] =
      node match
        case Semi(None) => None
        case Semi(Some(inner)) =>
          unwrapSemi(inner) match
            case None => None
            case Some(valueNode: Val) => local(valueNode, UntypedValueKind.Val)
            case Some(valueNode: Var) => local(valueNode, UntypedValueKind.Var)
            case Some(other) =>
              expr(other).map(value => UntypedExprStmt(value, nodeSpan(node)))
        case other =>
          unwrapSemi(other) match
            case None => None
            case Some(valueNode: Val) => local(valueNode, UntypedValueKind.Val)
            case Some(valueNode: Var) => local(valueNode, UntypedValueKind.Var)
            case Some(value)          => expr(value)

    private def local(
        node: Val,
        kind: UntypedValueKind,
    ): Option[UntypedLocal] =
      val span = nodeSpan(node)
      val valueType = node.ty.fold[Option[Option[UntypedType]]](Some(None)) { ty =>
        typeFromNode(ty, Some(span)).map(Some(_))
      }
      val init = node.init.fold[Option[Option[UntypedExpr]]](Some(None)) { value =>
        expr(value).map(Some(_))
      }
      valueType.zip(init).map { case (t, i) =>
        UntypedLocal(kind, node.name.name, t, i, span)
      }

    private def local(
        node: Var,
        kind: UntypedValueKind,
    ): Option[UntypedLocal] =
      val span = nodeSpan(node)
      val valueType = node.ty.fold[Option[Option[UntypedType]]](Some(None)) { ty =>
        typeFromNode(ty, Some(span)).map(Some(_))
      }
      val init = node.init.fold[Option[Option[UntypedExpr]]](Some(None)) { value =>
        expr(value).map(Some(_))
      }
      valueType.zip(init).map { case (t, i) =>
        UntypedLocal(kind, node.name.name, t, i, span)
      }

    private def expr(node: syntax.Node): Option[UntypedExpr] =
      unwrapSemi(node) match
        case None => Some(UntypedUnitLiteral(nodeSpan(node)))
        case Some(block: Block) =>
          val items = block.stmts.map(blockItem)
          sequence(items).map(UntypedBlock(_, nodeSpan(block)))
        case Some(name: Ident) =>
          Some(UntypedName(UntypedPath(List(name.name), nodeSpan(name)), nodeSpan(name)))
        case Some(BoolLit(value)) =>
          Some(UntypedBoolLiteral(value, nodeSpan(node)))
        case Some(IntLit(value)) =>
          Some(UntypedIntLiteral(value, nodeSpan(node)))
        case Some(FloatLit(value)) =>
          Some(UntypedFloatLiteral(value, nodeSpan(node)))
        case Some(StrLit(value)) =>
          Some(UntypedStringLiteral(value, nodeSpan(node)))
        case Some(AsciiLit(value)) =>
          Some(UntypedAsciiLiteral(value, nodeSpan(node)))
        case Some(RuneLit(value)) =>
          Some(UntypedRuneLiteral(value, nodeSpan(node)))
        case Some(Select(lhs, rhs, false)) =>
          expr(lhs).map(receiver => UntypedSelect(receiver, rhs.name, nodeSpan(node)))
        case Some(Select(lhs, rhs, true)) =>
          typeFromNode(lhs, Some(nodeSpan(node))).map(owner =>
            UntypedVariantConstructor(owner, rhs.name, nodeSpan(node)),
          )
        case Some(Apply(Select(_, rhs, false), _, false))
            if higherOrderMethodNames.contains(rhs.name) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.higher-order-api",
            s"${rhs.name} is outside the initial cosmo0 higher-order API subset",
          )
        case Some(Apply(typeApply @ Apply(_, _, true), args, false)) =>
          val callee = typeFromNode(typeApply, Some(nodeSpan(typeApply))).map(t =>
            UntypedTypeConstructor(t, nodeSpan(typeApply)),
          )
          val callArgs = args.map(expr)
          for
            c <- callee
            as <- sequence(callArgs)
          yield UntypedCall(c, as, nodeSpan(node))
        case Some(Apply(lhs, args, false)) =>
          val callee = expr(lhs)
          val callArgs = args.map(expr)
          for
            c <- callee
            as <- sequence(callArgs)
          yield UntypedCall(c, as, nodeSpan(node))
        case Some(applyNode @ Apply(_, _, true)) =>
          unsupported(
            applyNode,
            "cosmo0.elaborate.unsupported.compile-time-apply",
            "compile-time type application is only supported in cosmo0 type positions",
          )
        case Some(BinOp(op, lhs, rhs)) if assignmentOps.contains(op) =>
          for
            target <- expr(lhs)
            value <- expr(rhs)
          yield UntypedAssign(target, value, op, nodeSpan(node))
        case Some(BinOp(op, lhs, rhs)) =>
          for
            left <- expr(lhs)
            right <- expr(rhs)
          yield UntypedBinary(op, left, right, nodeSpan(node))
        case Some(UnOp("mut", _)) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.mut-expression",
            "mut is only supported in cosmo0 reference type syntax",
          )
        case Some(UnOp(op, lhs)) =>
          expr(lhs).map(value => UntypedUnary(op, value, nodeSpan(node)))
        case Some(If(cond, thenBranch, elseBranch)) =>
          val span = nodeSpan(node)
          val elseValue = elseBranch.fold[Option[Option[UntypedExpr]]](Some(None)) { branch =>
            expr(branch).map(Some(_))
          }
          expr(cond).zip(expr(thenBranch)).zip(elseValue).map { case ((c, t), e) =>
            UntypedIf(c, t, e, span)
          }
        case Some(Loop(body)) =>
          expr(body).map(UntypedLoop(_, nodeSpan(node)))
        case Some(While(cond, body)) =>
          for
            c <- expr(cond)
            b <- expr(body)
          yield UntypedWhile(c, b, nodeSpan(node))
        case Some(For(name, iter, body)) =>
          for
            i <- expr(iter)
            b <- expr(body)
          yield UntypedFor(name.name, i, b, nodeSpan(node))
        case Some(Match(lhs, rhs: CaseBlock)) =>
          val arms = rhs.stmts.map(matchArm)
          for
            scrutinee <- expr(lhs)
            as <- sequence(arms)
          yield UntypedMatch(scrutinee, as, nodeSpan(node))
        case Some(Match(_, rhs)) =>
          unsupported(
            rhs,
            "cosmo0.elaborate.unsupported.match-body",
            "cosmo0 match expressions must contain only case arms",
          )
        case Some(Return(value)) =>
          expr(value).map(UntypedReturn(_, nodeSpan(node)))
        case Some(Break()) =>
          Some(UntypedBreak(nodeSpan(node)))
        case Some(Continue()) =>
          Some(UntypedContinue(nodeSpan(node)))
        case Some(lambda: Lambda) =>
          unsupported(
            lambda,
            "cosmo0.elaborate.unsupported.lambda",
            "lambdas and closures are outside the initial cosmo0 subset",
          )
        case Some(asNode: As) =>
          unsupported(
            asNode,
            "cosmo0.elaborate.unsupported.cast",
            "as-casts are outside the initial cosmo0 subset",
          )
        case Some(tmpl: TmplApply) =>
          unsupported(
            tmpl,
            "cosmo0.elaborate.unsupported.template-literal",
            "template literals are outside the initial cosmo0 subset",
          )
        case Some(args: ArgsLit) =>
          unsupported(
            args,
            "cosmo0.elaborate.unsupported.tuple",
            "tuple and named-argument literals are outside the initial cosmo0 subset",
          )
        case Some(params: ParamsLit) =>
          unsupported(
            params,
            "cosmo0.elaborate.unsupported.params-literal",
            "parameter literals are outside the initial cosmo0 subset",
          )
        case Some(keyed: KeyedArg) =>
          unsupported(
            keyed,
            "cosmo0.elaborate.unsupported.named-argument",
            "named arguments are outside the initial cosmo0 subset",
          )
        case Some(TodoLit) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.todo",
            "??? placeholders are outside the initial cosmo0 subset",
          )
        case Some(decorated: Decorate) =>
          unsupported(
            decorated,
            "cosmo0.elaborate.unsupported.decorator",
            "decorators and staging annotations are outside the initial cosmo0 subset",
          )
        case Some(_: Val | _: Var | _: Typ | _: Def | _: Class | _: Impl | _: Import | _: Case) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.declaration-expression",
            s"${constructName(node)} cannot appear as a cosmo0 expression",
          )
        case Some(caseBlock: CaseBlock) =>
          unsupported(
            caseBlock,
            "cosmo0.elaborate.unsupported.case-block",
            "case blocks are only supported as match bodies",
          )
        case Some(err: Err) =>
          unsupported(
            err,
            "cosmo0.elaborate.unsupported.parser-error",
            err.msg,
          )
        case Some(other) =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.expression",
            s"${constructName(other)} is outside the initial cosmo0 expression subset",
          )

    private def matchArm(node: Case): Option[UntypedMatchArm] =
      val span = nodeSpan(node)
      val body = node.body.fold[Option[Option[UntypedExpr]]](Some(None)) { bodyNode =>
        expr(bodyNode).map(Some(_))
      }
      pattern(node.cond).zip(body).map { case (patternValue, bodyValue) =>
        UntypedMatchArm(patternValue, bodyValue, span)
      }

    private def pattern(node: syntax.Node): Option[UntypedPattern] =
      node match
        case Ident("_") =>
          Some(UntypedWildcardPattern(nodeSpan(node)))
        case Ident(name) =>
          Some(UntypedBindingPattern(name, nodeSpan(node)))
        case BoolLit(value) =>
          Some(UntypedBoolLiteral(value, nodeSpan(node)))
        case IntLit(value) =>
          Some(UntypedIntLiteral(value, nodeSpan(node)))
        case FloatLit(value) =>
          Some(UntypedFloatLiteral(value, nodeSpan(node)))
        case StrLit(value) =>
          Some(UntypedStringLiteral(value, nodeSpan(node)))
        case AsciiLit(value) =>
          Some(UntypedAsciiLiteral(value, nodeSpan(node)))
        case RuneLit(value) =>
          Some(UntypedRuneLiteral(value, nodeSpan(node)))
        case Apply(callee, args, false) =>
          val constructor = expr(callee)
          val argPatterns = args.map(pattern)
          for
            c <- constructor
            as <- sequence(argPatterns)
          yield UntypedVariantPattern(c, as, nodeSpan(node))
        case select: Select =>
          expr(select).map(constructor => UntypedVariantPattern(constructor, Nil, nodeSpan(node)))
        case other =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.pattern",
            s"${constructName(other)} is not a supported cosmo0 pattern",
          )

    private def typeFromNode(
        node: syntax.Node,
        fallbackSpan: Option[SourceSpan] = None,
    ): Option[UntypedType] =
      node match
        case Ident("Type") =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.host-type",
            "host Type and type-level programming are outside the initial cosmo0 subset",
          )
        case UnOp("&", UnOp("mut", target)) =>
          typeFromNode(target, fallbackSpan).map(t =>
            UntypedRefType(t, mutable = true, nodeSpan(node, fallbackSpan)),
          )
        case UnOp("&", target) =>
          typeFromNode(target, fallbackSpan).map(t =>
            UntypedRefType(t, mutable = false, nodeSpan(node, fallbackSpan)),
          )
        case Apply(lhs, args, true) =>
          pathFromNode(lhs, fallbackSpan) match
            case Some(base) if standardGenericNames.contains(base.parts.lastOption.getOrElse("")) =>
              val typeArgs = args.map(typeFromNode(_, Some(nodeSpan(node, fallbackSpan))))
              sequence(typeArgs).map { values =>
                base.parts.lastOption match
                  case Some("Ref") if values.size == 1 =>
                    UntypedRefType(values.head, mutable = false, nodeSpan(node, fallbackSpan))
                  case Some("RefMut") if values.size == 1 =>
                    UntypedRefType(values.head, mutable = true, nodeSpan(node, fallbackSpan))
                  case _ =>
                    UntypedAppliedType(base, values, nodeSpan(node, fallbackSpan))
              }
            case Some(base) =>
              unsupported(
                node,
                "cosmo0.elaborate.unsupported.generic-type",
                s"${base.text} is not a registered cosmo0 standard generic type",
              )
            case None => None
        case Apply(_, _, false) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.runtime-type-apply",
            "cosmo0 type applications must use compile-time square-bracket syntax",
          )
        case pathNode =>
          pathFromNode(pathNode, fallbackSpan).map(path =>
            UntypedNamedType(path, nodeSpan(pathNode, fallbackSpan)),
          )

    private def pathFromNode(
        node: syntax.Node,
        fallbackSpan: Option[SourceSpan] = None,
    ): Option[UntypedPath] =
      node match
        case Ident(name) =>
          Some(UntypedPath(List(name), nodeSpan(node, fallbackSpan)))
        case Select(lhs, rhs, _) =>
          pathFromNode(lhs, fallbackSpan).map(path =>
            UntypedPath(path.parts :+ rhs.name, nodeSpan(node, fallbackSpan)),
          )
        case other =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.path",
            s"${constructName(other)} cannot be used as a cosmo0 path",
          )

    def nodeSpan(node: syntax.Node): SourceSpan =
      nodeSpan(node, None)

    private def nodeSpan(
        node: syntax.Node,
        fallbackSpan: Option[SourceSpan],
    ): SourceSpan =
      if node.offset >= 0 && node.end >= node.offset then source.span(node.offset, node.end)
      else fallbackSpan.getOrElse(source.span(0, 0))

    private def declarationVisibility(node: syntax.Node): UntypedVisibility =
      val start = node.offset.max(0).min(source.text.length)
      val text = source.text.drop(start).dropWhile(_.isWhitespace)
      if startsWithWord(text, "private") then UntypedVisibility.Private
      else UntypedVisibility.Public

    private def startsWithWord(text: String, word: String): Boolean =
      text.startsWith(word) && text.lift(word.length).forall(ch => !isIdentContinue(ch))

    private def isIdentContinue(char: Char): Boolean =
      char.isLetterOrDigit || char == '_'

    private def unwrapSemi(node: syntax.Node): Option[syntax.Node] =
      node match
        case Semi(None)        => None
        case Semi(Some(inner)) => unwrapSemi(inner)
        case other             => Some(other)

    private def hasExplicitTypeParams(params: Option[List[Param]]): Boolean =
      params.exists(_.exists(_.ct))

    private def sequence[A](items: List[Option[A]]): Option[List[A]] =
      val values = ListBuffer.empty[A]
      var ok = true
      items.foreach {
        case Some(value) => values += value
        case None        => ok = false
      }
      if ok then Some(values.toList) else None

    private def unsupported[A](
        node: syntax.Node,
        code: String,
        message: String,
    ): Option[A] =
      report(node, code, message)
      None

    private def report(
        node: syntax.Node,
        code: String,
        message: String,
    ): Unit =
      diagnostics += Diagnostic(
        Phase.Check,
        DiagnosticSeverity.Error,
        code,
        message,
        Some(nodeSpan(node)),
      )

    private def constructName(node: syntax.Node): String =
      node.getClass.getSimpleName.stripSuffix("$")
