package cosmo0

/** Source-level type model used by the cosmo0 checker and later stages.
  *
  * `SourceType` is the resolved form of `UntypedType`. It is still a
  * source-facing type, not a lowered runtime type: aliases may be retained for
  * display, standard library families are represented by descriptor names, and
  * foreign C++ symbols keep canonical C++ names.
  *
  * Examples:
  *
  * {{{
  * i32               => SourceType.Builtin("i32")
  * String            => SourceType.Builtin("String")
  * UserClass         => SourceType.User("UserClass")
  * type Count = i32  => SourceType.Alias("Count", SourceType.I32)
  * Vec[i32]          => SourceType.Standard("Vec", List(SourceType.I32))
  * &mut String       => SourceType.Ref(SourceType.String, mutable = true)
  * (i32) -> Bool     => SourceType.Function(List(SourceType.I32), Bool)
  * std.vector[i32]   => SourceType.ForeignApplied("::std::vector", List(i32))
  * }}}
  *
  * Inference and checking helpers anchored here:
  *
  *   - `dealias` removes alias wrappers before category checks, descriptor
  *     lookup, and MLTT source type relation construction.
  *   - Numeric inference uses `isInteger`, `isFloat`, and `isNumeric`; the
  *     typer supplies expected numeric types to literals and arithmetic
  *     operands.
  *   - References preserve mutability. `&mut T` can flow to `&T`, but `&T`
  *     cannot flow to `&mut T`.
  */
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

  /** Human-readable source spelling used in diagnostics and artifact summaries.
    */
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

/** Constructors, scalar aliases, normalization, and predicates for
  * `SourceType`.
  */
object SourceType:
  /** Builtin Unit type. */
  val Unit: SourceType = Builtin("Unit")

  /** Builtin Bool type. */
  val Bool: SourceType = Builtin("Bool")

  /** Default signed integer type. */
  val I32: SourceType = Builtin("i32")

  /** Platform-sized unsigned integer type. */
  val Usize: SourceType = Builtin("usize")

  /** Builtin UTF string type. */
  val String: SourceType = Builtin("String")

  /** Builtin character type. */
  val Char: SourceType = Builtin("Char")

  /** Builtin byte type, also used for ASCII literals. */
  val Byte: SourceType = Builtin("u8")

  /** Builtin rune type, also used for rune literals. */
  val Rune: SourceType = Builtin("u32")

  /** Default floating-point type. */
  val F64: SourceType = Builtin("f64")

  /** Scalar type names accepted by source type resolution. */
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

  /** Returns the scalar type for a source name, if the name is builtin. */
  def scalar(name: String): Option[SourceType] =
    scalarTypes.get(name)

  /** Removes alias wrappers recursively while preserving all other structure.
    *
    * This is the first step for category checks, most descriptor lookup, and
    * MLTT source type relation construction.
    */
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

  /** True for builtin signed and unsigned integer types. */
  def isInteger(value: SourceType): Boolean =
    dealias(value) match
      case Builtin(
            "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
            "usize",
          ) =>
        true
      case _ => false

  /** True for builtin floating-point types. */
  def isFloat(value: SourceType): Boolean =
    dealias(value) match
      case Builtin("f32" | "f64") => true
      case _                      => false

  /** True for integer or floating-point types. */
  def isNumeric(value: SourceType): Boolean =
    isInteger(value) || isFloat(value)

  /** Returns the referenced type when `value` is a reference, otherwise the
    * dealiased value itself.
    */
  def deref(value: SourceType): SourceType =
    dealias(value) match
      case Ref(target, _) => target
      case other          => other

  /** Returns the mutability of a reference type, if `value` is a reference. */
  def refMutable(value: SourceType): Option[Boolean] =
    dealias(value) match
      case Ref(_, mutable) => Some(mutable)
      case _               => None

/** Template type used by standard generic descriptors.
  *
  * Descriptor signatures are written once with positional type arguments and
  * instantiated against an owner type. For example, the descriptor for
  * `Vec[T].get(index: usize): T` stores `T` as `Arg(0)` and instantiates it
  * with the element type from `Vec[i32]`.
  */
sealed trait SourceTypeTemplate:
  def instantiate(args: List[SourceType]): SourceType

object SourceTypeTemplate:
  /** Positional type argument from the owner type. */
  final case class Arg(index: Int) extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      args.lift(index).getOrElse(SourceType.Error)

  /** Concrete type that does not depend on owner type arguments. */
  final case class Concrete(value: SourceType) extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType = value

  /** Reference template around another template. */
  final case class Ref(target: SourceTypeTemplate, mutable: Boolean)
      extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      SourceType.Ref(target.instantiate(args), mutable)

  /** Standard generic template whose arguments are instantiated recursively. */
  final case class Standard(name: String, values: List[SourceTypeTemplate])
      extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      SourceType.Standard(name, values.map(_.instantiate(args)))

/** Callable parameter after type resolution. */
final case class CallableParam(
    name: String,
    valueType: SourceType,
    span: SourceSpan,
)

/** Method receiver metadata. `mutable = true` requires a mutation-capable
  * receiver expression.
  */
final case class CallableReceiver(
    valueType: SourceType,
    mutable: Boolean,
)

/** Resolved callable signature for functions, methods, constructors, and
  * function-typed values.
  *
  * `params` excludes the receiver for methods. `functionType` is the callable
  * value type seen by selections and names.
  */
final case class CallableSignature(
    name: String,
    params: List[CallableParam],
    returnType: SourceType,
    receiver: Option[CallableReceiver] = None,
):
  def functionType: SourceType =
    SourceType.Function(params.map(_.valueType), returnType)

/** Descriptor parameter before generic owner instantiation. */
final case class DescriptorParam(
    name: String,
    valueType: SourceTypeTemplate,
)

/** Descriptor callable before generic owner instantiation. */
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

/** Descriptor for a built-in or standard generic family.
  *
  * Descriptors are the source-level method and constructor table used by the
  * typer. They define what calls are allowed without requiring ordinary source
  * declarations for the runtime families.
  */
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

/** Registry for cosmo0 standard descriptors.
  *
  * The registry intentionally exposes only the primitive and temporary standard
  * families supported by the current checker/runtime boundary. New runtime
  * families should be added to the boundary list deliberately so they cannot
  * become available through descriptor drift.
  */
object StandardGenericDescriptors:
  /** Boundary between the cosmo0 source subset and runtime descriptor surface.
    *
    * `allowedDescriptorNames` is deliberately smaller than the possible runtime
    * library surface. This keeps the checker from accepting source APIs that
    * lowering or support libraries do not yet guarantee.
    */
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

  /** All descriptor families available to source type checking. */
  val all: Map[String, StandardGenericDescriptor] =
    implementationDescriptors
      .map(descriptor => descriptor.name -> descriptor)
      .toMap

  /** User-facing standard generic families.
    *
    * Examples:
    *
    *   - `Vec[T]` has `<init>`, `push`, `get`, `set`, `len`, and related
    *     collection methods.
    *   - `Option[T]` has `Some`, `None`, and predicate methods.
    *   - `Result[T, U]` has `Ok`, `Err`, and predicate methods.
    *   - `Ref[T]` and `RefMut[T]` are parsed as reference types, but
    *     descriptors remain registered as temporary family names for boundary
    *     validation.
    */
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

  /** Primitive runtime descriptors. They provide method-call syntax for
    * builtins such as numeric arithmetic helpers and String operations.
    */
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

  /** Looks up a descriptor family by source name. */
  def get(name: String): Option[StandardGenericDescriptor] =
    all.get(name)

  /** Fails fast if the implementation registers a descriptor family outside the
    * intended cosmo0 boundary.
    */
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

  /** Numeric method surface shared by integer and float descriptors. */
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

  /** Comparable method surface shared by ordered primitive descriptors. */
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

  /** Builds a descriptor and indexes constructors/methods by source call name.
    */
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

  /** Compact constructor for descriptor callables. */
  private def call(
      name: String,
      params: List[DescriptorParam],
      returnType: SourceTypeTemplate,
      receiverMutable: Boolean = false,
  ): DescriptorCallable =
    DescriptorCallable(name, params, returnType, receiverMutable)

  /** Compact constructor for descriptor parameters. */
  private def param(
      name: String,
      valueType: SourceTypeTemplate,
  ): DescriptorParam =
    DescriptorParam(name, valueType)
