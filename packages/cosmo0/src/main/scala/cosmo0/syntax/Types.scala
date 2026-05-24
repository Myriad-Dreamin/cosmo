package cosmo0

enum SourceType:
  case Builtin(name: String)
  case User(name: String)
  case Alias(name: String, target: SourceType)
  case TypeParam(name: String)
  case ForeignNamespace(value: SourceCppNamespaceImport)
  case ForeignSymbol(canonicalName: String)
  case ForeignApplied(canonicalName: String, args: List[SourceType])
  case Ref(target: SourceType, mutable: Boolean)
  case Standard(name: String, args: List[SourceType])
  case Function(params: List[SourceType], returnType: SourceType)
  case Never
  case Error

  def display: String =
    this match
      case SourceType.Builtin(name)                => name
      case SourceType.User(name)                   => name
      case SourceType.Alias(name, _)               => name
      case SourceType.TypeParam(name)              => name
      case SourceType.ForeignNamespace(value)      => value.alias
      case SourceType.ForeignSymbol(canonicalName) => canonicalName
      case SourceType.ForeignApplied(canonicalName, args) =>
        s"$canonicalName[${args.map(_.display).mkString(", ")}]"
      case SourceType.Ref(target, true) =>
        s"&mut ${target.display}"
      case SourceType.Ref(target, false) =>
        s"&${target.display}"
      case SourceType.Standard(name, args) =>
        s"$name[${args.map(_.display).mkString(", ")}]"
      case SourceType.Function(params, returnType) =>
        s"(${params.map(_.display).mkString(", ")}) -> ${returnType.display}"
      case SourceType.Never => "never"
      case SourceType.Error => "<error>"

object SourceType:
  val Unit: SourceType = Builtin("Unit")
  val Bool: SourceType = Builtin("Bool")
  val I32: SourceType = Builtin("i32")
  val Usize: SourceType = Builtin("usize")
  val String: SourceType = Builtin("String")
  val Char: SourceType = Builtin("Char")
  val Byte: SourceType = Builtin("u8")
  val Rune: SourceType = Builtin("u32")
  val F64: SourceType = Builtin("f64")

  val scalarTypes: Map[String, SourceType] =
    Map(
      "Unit" -> Unit,
      "Bool" -> Bool,
      "bool" -> Bool,
      "i8" -> Builtin("i8"),
      "i16" -> Builtin("i16"),
      "i32" -> I32,
      "i64" -> Builtin("i64"),
      "u8" -> Byte,
      "u16" -> Builtin("u16"),
      "u32" -> Rune,
      "u64" -> Builtin("u64"),
      "usize" -> Usize,
      "Byte" -> Byte,
      "Char" -> Char,
      "String" -> String,
      "str" -> String,
      "f32" -> Builtin("f32"),
      "f64" -> F64,
    )

  def scalar(name: String): Option[SourceType] =
    scalarTypes.get(name)

  def dealias(value: SourceType): SourceType =
    value match
      case Alias(_, target)             => dealias(target)
      case TypeParam(name)              => TypeParam(name)
      case ForeignNamespace(value)      => ForeignNamespace(value)
      case ForeignSymbol(canonicalName) => ForeignSymbol(canonicalName)
      case ForeignApplied(canonicalName, args) =>
        ForeignApplied(canonicalName, args.map(dealias))
      case Ref(target, mutable) =>
        Ref(dealias(target), mutable)
      case Standard(name, args) =>
        Standard(name, args.map(dealias))
      case Function(params, returnType) =>
        Function(params.map(dealias), dealias(returnType))
      case other => other

  def same(left: SourceType, right: SourceType): Boolean =
    (dealias(left), dealias(right)) match
      case (Error, _) | (_, Error)      => true
      case (Never, _) | (_, Never)      => true
      case (Builtin(a), Builtin(b))     => a == b
      case (User(a), User(b))           => a == b
      case (TypeParam(a), TypeParam(b)) => a == b
      case (ForeignNamespace(a), ForeignNamespace(b)) =>
        a.alias == b.alias && a.namespace == b.namespace && a.headers == b.headers
      case (ForeignSymbol(a), ForeignSymbol(b)) => a == b
      case (ForeignApplied(na, aa), ForeignApplied(nb, ab)) =>
        na == nb && aa.length == ab.length && aa.zip(ab).forall { case (a, b) =>
          same(a, b)
        }
      case (Ref(a, ma), Ref(b, mb)) => ma == mb && same(a, b)
      case (Standard(na, aa), Standard(nb, ab)) =>
        na == nb && aa.length == ab.length && aa.zip(ab).forall { case (a, b) =>
          same(a, b)
        }
      case (Function(ap, ar), Function(bp, br)) =>
        ap.length == bp.length && ap.zip(bp).forall { case (a, b) =>
          same(a, b)
        } && same(ar, br)
      case _ => false

  def assignable(from: SourceType, to: SourceType): Boolean =
    same(from, to) ||
      ((dealias(from), dealias(to)) match
        case (Ref(fromTarget, true), Ref(toTarget, false)) =>
          same(fromTarget, toTarget)
        case _ =>
          false
      )

  def isInteger(value: SourceType): Boolean =
    dealias(value) match
      case Builtin(
            "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
            "usize",
          ) =>
        true
      case _ => false

  def isFloat(value: SourceType): Boolean =
    dealias(value) match
      case Builtin("f32" | "f64") => true
      case _                      => false

  def isNumeric(value: SourceType): Boolean =
    isInteger(value) || isFloat(value)

  def deref(value: SourceType): SourceType =
    dealias(value) match
      case Ref(target, _) => target
      case other          => other

  def refMutable(value: SourceType): Option[Boolean] =
    dealias(value) match
      case Ref(_, mutable) => Some(mutable)
      case _               => None

sealed trait SourceTypeTemplate:
  def instantiate(args: List[SourceType]): SourceType

object SourceTypeTemplate:
  final case class Arg(index: Int) extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      args.lift(index).getOrElse(SourceType.Error)

  final case class Concrete(value: SourceType) extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType = value

  final case class Ref(target: SourceTypeTemplate, mutable: Boolean)
      extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      SourceType.Ref(target.instantiate(args), mutable)

  final case class Standard(name: String, values: List[SourceTypeTemplate])
      extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      SourceType.Standard(name, values.map(_.instantiate(args)))

final case class CallableParam(
    name: String,
    valueType: SourceType,
    span: SourceSpan,
)

final case class CallableReceiver(
    valueType: SourceType,
    mutable: Boolean,
)

final case class CallableSignature(
    name: String,
    params: List[CallableParam],
    returnType: SourceType,
    receiver: Option[CallableReceiver] = None,
):
  def functionType: SourceType =
    SourceType.Function(params.map(_.valueType), returnType)

final case class DescriptorParam(
    name: String,
    valueType: SourceTypeTemplate,
)

final case class DescriptorCallable(
    name: String,
    params: List[DescriptorParam],
    returnType: SourceTypeTemplate,
    receiverMutable: Boolean = false,
):
  def instantiate(owner: SourceType, span: SourceSpan): CallableSignature =
    val typeArgs =
      SourceType.dealias(owner) match
        case SourceType.Standard(_, args) => args
        case _                            => Nil
    CallableSignature(
      name,
      params.map(param =>
        CallableParam(param.name, param.valueType.instantiate(typeArgs), span),
      ),
      returnType.instantiate(typeArgs),
      Some(CallableReceiver(owner, receiverMutable)),
    )

final case class StandardGenericDescriptor(
    name: String,
    arity: Int,
    constructors: Map[String, DescriptorCallable],
    methods: Map[String, DescriptorCallable],
):
  def constructor(name: String): Option[DescriptorCallable] =
    constructors.get(name)

  def method(name: String): Option[DescriptorCallable] =
    methods.get(name)

object StandardGenericDescriptors:
  object Boundary:
    val primitiveRuntimeDescriptorNames: Set[String] =
      Set(
        "Bool",
        "Char",
        "String",
        "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "usize",
        "f32",
        "f64",
      )

    val temporaryStandardDescriptorNames: Set[String] =
      Set(
        "Vec",
        "Option",
        "Result",
        "Arena",
        "Id",
        "Map",
        "Set",
        "Ptr",
        "Box",
        "Ref",
        "RefMut",
      )

    val rejectedRuntimeDescriptorFamilies: Set[String] =
      Set(
        "Json",
        "JsonValue",
        "Filesystem",
        "FileSystem",
        "Fs",
        "Command",
        "Process",
        "StringBuilder",
        "TextBuilder",
        "TextView",
        "TextSlice",
        "SourceText",
        "SourceMap",
        "BigInt",
        "BigDecimal",
      )

    val allowedDescriptorNames: Set[String] =
      primitiveRuntimeDescriptorNames ++ temporaryStandardDescriptorNames

    def isAllowedDescriptorName(name: String): Boolean =
      allowedDescriptorNames.contains(name)

  private val T = SourceTypeTemplate.Arg(0)
  private val U = SourceTypeTemplate.Arg(1)
  private val UnitT = SourceTypeTemplate.Concrete(SourceType.Unit)
  private val BoolT = SourceTypeTemplate.Concrete(SourceType.Bool)
  private val UsizeT = SourceTypeTemplate.Concrete(SourceType.Usize)
  private val StringT = SourceTypeTemplate.Concrete(SourceType.String)
  private val CharT = SourceTypeTemplate.Concrete(SourceType.Char)
  private val ByteT = SourceTypeTemplate.Concrete(SourceType.Byte)

  private val implementationDescriptors: List[StandardGenericDescriptor] =
    standardGenericDescriptors ++ runtimeDescriptors

  validateDescriptorBoundary(implementationDescriptors)

  val all: Map[String, StandardGenericDescriptor] =
    implementationDescriptors
      .map(descriptor => descriptor.name -> descriptor)
      .toMap

  private def standardGenericDescriptors: List[StandardGenericDescriptor] =
    List(
      descriptor(
        "Vec",
        1,
        constructors = List(
          call("<init>", Nil, SourceTypeTemplate.Standard("Vec", List(T))),
        ),
        methods = List(
          call("push", List(param("value", T)), UnitT, receiverMutable = true),
          call("get", List(param("index", UsizeT)), T),
          call(
            "set",
            List(param("index", UsizeT), param("value", T)),
            UnitT,
            receiverMutable = true,
          ),
          call("size", Nil, UsizeT),
          call("len", Nil, UsizeT),
          call("is_empty", Nil, BoolT),
        ),
      ),
      descriptor(
        "Option",
        1,
        constructors = List(
          call(
            "Some",
            List(param("value", T)),
            SourceTypeTemplate.Standard("Option", List(T)),
          ),
          call("None", Nil, SourceTypeTemplate.Standard("Option", List(T))),
        ),
        methods = List(
          call("is_some", Nil, BoolT),
          call("is_none", Nil, BoolT),
        ),
      ),
      descriptor(
        "Result",
        2,
        constructors = List(
          call(
            "Ok",
            List(param("value", T)),
            SourceTypeTemplate.Standard("Result", List(T, U)),
          ),
          call(
            "Err",
            List(param("error", U)),
            SourceTypeTemplate.Standard("Result", List(T, U)),
          ),
        ),
        methods = List(
          call("is_ok", Nil, BoolT),
          call("is_err", Nil, BoolT),
        ),
      ),
      descriptor(
        "Arena",
        1,
        constructors = List(
          call("<init>", Nil, SourceTypeTemplate.Standard("Arena", List(T))),
        ),
        methods = List(
          call(
            "alloc",
            List(param("value", T)),
            SourceTypeTemplate.Standard("Id", List(T)),
            receiverMutable = true,
          ),
          call(
            "get",
            List(param("id", SourceTypeTemplate.Standard("Id", List(T)))),
            SourceTypeTemplate.Ref(T, mutable = false),
          ),
          call(
            "get_mut",
            List(param("id", SourceTypeTemplate.Standard("Id", List(T)))),
            SourceTypeTemplate.Ref(T, mutable = true),
            receiverMutable = true,
          ),
          call("size", Nil, UsizeT),
          call("len", Nil, UsizeT),
        ),
      ),
      descriptor("Id", 1, constructors = Nil, methods = Nil),
      descriptor(
        "Map",
        2,
        constructors = List(
          call("<init>", Nil, SourceTypeTemplate.Standard("Map", List(T, U))),
        ),
        methods = List(
          call(
            "get",
            List(param("key", T)),
            SourceTypeTemplate.Standard("Option", List(U)),
          ),
          call(
            "insert",
            List(param("key", T), param("value", U)),
            SourceTypeTemplate.Standard("Option", List(U)),
            receiverMutable = true,
          ),
          call("contains", List(param("key", T)), BoolT),
          call("contains_key", List(param("key", T)), BoolT),
          call("size", Nil, UsizeT),
          call("len", Nil, UsizeT),
          call("is_empty", Nil, BoolT),
        ),
      ),
      descriptor(
        "Set",
        1,
        constructors = List(
          call("<init>", Nil, SourceTypeTemplate.Standard("Set", List(T))),
        ),
        methods = List(
          call(
            "insert",
            List(param("value", T)),
            UnitT,
            receiverMutable = true,
          ),
          call("contains", List(param("value", T)), BoolT),
          call("size", Nil, UsizeT),
          call("len", Nil, UsizeT),
          call("is_empty", Nil, BoolT),
        ),
      ),
      descriptor(
        "Ptr",
        1,
        constructors = List(
          call(
            "<init>",
            List(param("value", SourceTypeTemplate.Ref(T, false))),
            SourceTypeTemplate.Standard("Ptr", List(T)),
          ),
        ),
        methods = Nil,
      ),
      descriptor(
        "Box",
        1,
        constructors = List(
          call(
            "<init>",
            List(param("value", T)),
            SourceTypeTemplate.Standard("Box", List(T)),
          ),
        ),
        methods = List(
          call("get", Nil, SourceTypeTemplate.Ref(T, mutable = false)),
          call(
            "get_mut",
            Nil,
            SourceTypeTemplate.Ref(T, mutable = true),
            receiverMutable = true,
          ),
        ),
      ),
      descriptor("Ref", 1, constructors = Nil, methods = Nil),
      descriptor("RefMut", 1, constructors = Nil, methods = Nil),
    )

  private def runtimeDescriptors: List[StandardGenericDescriptor] =
    List(
      descriptor(
        "Bool",
        0,
        constructors = Nil,
        methods = List(
          call("not", Nil, BoolT),
          call("and", List(param("right", BoolT)), BoolT),
          call("or", List(param("right", BoolT)), BoolT),
          call("eq", List(param("right", BoolT)), BoolT),
          call("ne", List(param("right", BoolT)), BoolT),
        ),
      ),
      descriptor(
        "Char",
        0,
        constructors = Nil,
        methods = comparableMethods(CharT),
      ),
      descriptor(
        "String",
        0,
        constructors = Nil,
        methods = List(
          call("size", Nil, UsizeT),
          call("len", Nil, UsizeT),
          call("is_empty", Nil, BoolT),
          call(
            "slice",
            List(param("start", UsizeT), param("end", UsizeT)),
            StringT,
          ),
          call("char_at", List(param("index", UsizeT)), CharT),
          call("byte_at", List(param("index", UsizeT)), ByteT),
          call("concat", List(param("right", StringT)), StringT),
          call("eq", List(param("right", StringT)), BoolT),
          call("ne", List(param("right", StringT)), BoolT),
        ),
      ),
    ) ++ numericDescriptors

  private def numericDescriptors: List[StandardGenericDescriptor] =
    List(
      SourceType.Builtin("i8"),
      SourceType.Builtin("i16"),
      SourceType.I32,
      SourceType.Builtin("i64"),
      SourceType.Byte,
      SourceType.Builtin("u16"),
      SourceType.Rune,
      SourceType.Builtin("u64"),
      SourceType.Usize,
      SourceType.Builtin("f32"),
      SourceType.F64,
    ).map { valueType =>
      val template = SourceTypeTemplate.Concrete(valueType)
      descriptor(
        valueType.display,
        0,
        constructors = Nil,
        methods = numericMethods(template),
      )
    }

  def get(name: String): Option[StandardGenericDescriptor] =
    all.get(name)

  private def validateDescriptorBoundary(
      descriptors: List[StandardGenericDescriptor],
  ): Unit =
    val rejected =
      descriptors
        .map(_.name)
        .filter(Boundary.rejectedRuntimeDescriptorFamilies.contains)
    require(
      rejected.isEmpty,
      s"rejected runtime descriptor families registered: ${rejected.distinct.sorted.mkString(", ")}",
    )
    val unknown =
      descriptors.map(_.name).filterNot(Boundary.isAllowedDescriptorName)
    require(
      unknown.isEmpty,
      s"descriptor families are outside the cosmo0 primitive boundary: ${unknown.distinct.sorted.mkString(", ")}",
    )

  private def numericMethods(
      valueType: SourceTypeTemplate,
  ): List[DescriptorCallable] =
    List(
      call("neg", Nil, valueType),
      call("add", List(param("right", valueType)), valueType),
      call("sub", List(param("right", valueType)), valueType),
      call("mul", List(param("right", valueType)), valueType),
      call("div", List(param("right", valueType)), valueType),
      call("mod", List(param("right", valueType)), valueType),
    ) ++ comparableMethods(valueType)

  private def comparableMethods(
      valueType: SourceTypeTemplate,
  ): List[DescriptorCallable] =
    List(
      call("eq", List(param("right", valueType)), BoolT),
      call("ne", List(param("right", valueType)), BoolT),
      call("lt", List(param("right", valueType)), BoolT),
      call("le", List(param("right", valueType)), BoolT),
      call("gt", List(param("right", valueType)), BoolT),
      call("ge", List(param("right", valueType)), BoolT),
    )

  private def descriptor(
      name: String,
      arity: Int,
      constructors: List[DescriptorCallable],
      methods: List[DescriptorCallable],
  ): StandardGenericDescriptor =
    StandardGenericDescriptor(
      name,
      arity,
      constructors.map(call => call.name -> call).toMap,
      methods.map(call => call.name -> call).toMap,
    )

  private def call(
      name: String,
      params: List[DescriptorParam],
      returnType: SourceTypeTemplate,
      receiverMutable: Boolean = false,
  ): DescriptorCallable =
    DescriptorCallable(name, params, returnType, receiverMutable)

  private def param(
      name: String,
      valueType: SourceTypeTemplate,
  ): DescriptorParam =
    DescriptorParam(name, valueType)
