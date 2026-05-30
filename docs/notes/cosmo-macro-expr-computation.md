# Cosmo Macro System: Expr / Type / Compile-Time Computation 讨论稿

## 目的

这份文档不是最终 spec 条文，而是给 Cosmo macro system 做架构澄清，重点回答五个问题：

1. 为什么不能用一个统一内部 `Expr` 覆盖 `SourceExpr / AttrExpr / ConstValue / GeneratedExpr / TypedExpr`，以及为什么宏边界仍可以有专用 `Expr[T = Any]`。
2. 结合当前 cosmo0 实现，最小可落地的数据边界与 phase boundary 应该是什么。
3. compile-time computation 与 ordinary type checking / lowering 的边界应如何切，尤其是默认值、属性参数、derive provider 输入输出。
4. 内部 Macro IR、Wasm sandbox、Component Model、native JIT、dynamic linking 分别适不适合作为“语义层”与“执行后端层”。
5. 为了避免后续实现任意发挥，哪些中间产物、接口、禁止项、测试契约需要先冻结。

本文默认把当前分支里 `openspec/changes/introduce-cosmo0-macro-system` 与 `openspec/changes/add-derived-cli-library` 的 scratch 视为上游输入，不回退其未提交修改。

## 当前仓库约束

先把现状说清楚，否则后面的“理想设计”会脱离代码现实。

- `syntax/Elaborator.scala` 当前只把文件级 `@include(...)` 和顶层 `@extern(...)` 当作有意义的 decorator；类成员 decorator 仍直接报 unsupported。
- `source/Pipeline.scala` 当前 package check 主流程仍然是：逐模块 elaboration -> 包级去重/排序 -> 合并成一个 `UntypedModule` -> `MlttTyper` -> `LirLowerer`。
- `tyck/mltt/Typer.scala` 已经证明“声明形状收集”和“表达式检查”可以分离：它先做 alias / trait / class / impl / function collection，再进入 ordinary expression typing。
- `syntax/Untyped.scala` 和 `syntax/Typed.scala` 已经明确区分了 elaborated AST 与 checked AST，这本身就是“不要把所有阶段揉成一个树”的现有证据。
- `tyck/Profiles.scala` 里已经有 `macros`、`reflection`、`staging` feature 名称，但 MLTT profile 仍把它们视为 rejected feature。也就是说，宏不该被偷塞成 type checker 的暗门。

这几个点决定了一个现实结论：macro system 更像是 package/source pipeline 的新增阶段，而不是 parser 插件、type checker escape hatch，或者 lowering/backend 的特殊 case。

## 核心结论预览

推荐方向可以先压缩成几句话：

1. 不能把所有阶段对象统一成一个内部 `Expr`，因为这里混杂了不同 phase、不同信任等级、不同求值语义、不同可见性的数据。
2. 可以定义宏边界专用的 `Expr[T = Any]`：初期 `T = Any` 表示 untyped code value，未来再把 `T` 收紧为 typed quotation 的结果类型。
3. 宏边界应该冻结在 `DeclShape / AttrExpr / ConstValue / ReflectionMetadata / Expr[Any] / GeneratedDecl`，而不是暴露编译器内部 `UntypedExpr` 或 `TypedExpr`。
4. compile-time computation 应拆成两个服务：`ConstEval` 与 `ProviderEval`，不要把 ordinary type checker 变成半个解释器。
5. 宏扩展的最小插入点是“声明形状与签名已知，但 ordinary body typing 尚未开始”的位置；表达式宏则在 ordinary expression typing 前展开为待检查表达式。
6. 长期语义层应该是 Cosmo 自己的 `MacroProtocol + MacroIR + capability model`；Wasm / Component Model / native JIT 更适合作为执行与分发后端，而不是语言语义本身。
7. 要避免后续实现发散，必须先冻结 phase 顺序、数据 schema、禁止项、命名/hygiene 规则、以及 golden/negative test 契约。

下面把这些点展开。

## 为什么不能有一个统一内部 Expr

“统一 Expr”听上去很简洁，但在宏系统里几乎一定会变成边界污染。问题不在命名，而在语义。

这里需要先区分两个概念：

- 不推荐的设计：把 compiler 内部的 `SourceExpr / AttrExpr / ConstValue / GeneratedExpr / TypedExpr` 合并成一个大 `Expr`。
- 推荐的设计：在 macro protocol 里定义一个专用 `Expr[T = Any]`，表示“宏可以消费或生成的代码值”。

后者不是前者的复活。`Expr[T = Any]` 是宏边界 ADT；它可以作为过程宏 API 的稳定对象，但不能等同于 parser AST，也不能等同于 type checker 产物。

### 1. 这几个对象不是同一种东西

建议先区分下面五类对象：

- `SourceExpr`
  - 用户源码中的一般表达式。
  - 目标是保真地表示用户写了什么。
  - 允许出现普通调用、控制流、局部变量、模式匹配、以及未来更复杂的语言结构。
- `AttrExpr`
  - 只用于 decorator / attribute 参数的受限语法。
  - 目标是做“声明式配置”，不是开放完整程序执行。
  - 第一阶段只需要 literal、path、type ref、array、record、keyed argument。
- `ConstValue`
  - compile-time evaluator 的结果，是“值”，不是“树”。
  - 目标是被 provider、schema builder、diagnostic 逻辑稳定消费。
  - 必须有限、可序列化、可比较、可稳定显示。
- `GeneratedExpr`
  - 宏生成声明内部的表达式节点。
  - 目标是“等待 ordinary typer 检查”的生成代码，不是 trusted checked artifact。
  - 它的合法性要由 ordinary type checking 负责兜底。
- `TypedExpr`
  - ordinary type checker 产物。
  - 已经带有 resolved type、name resolution 结果、mutability、call signature 等编译器内部不变量。
  - 不应被 provider 直接构造，也不应作为 provider 的长期 ABI。

如果把它们合并为一个 compiler-internal 总 `Expr`，唯一的结果就是：每个使用方都要靠运行时约定去猜“当前拿到的到底是哪种 Expr”。这会立刻引出大量隐式前提。

### 2. 它们处于不同的 phase

phase distinction 不是实现细节，而是语义约束：

- `SourceExpr` 发生在 elaboration 之后、ordinary typing 之前。
- `AttrExpr` 只存在于 macro attribute / derive config 的语境。
- `ConstValue` 是 compile-time evaluation 之后的结果。
- `GeneratedExpr` 存在于 macro expansion 输出中，但还没有被 ordinary typing 信任。
- `TypedExpr` 只在 ordinary typing 完成后存在。

如果把这些阶段对象统一，宏 provider 很容易越过边界：

- 看到 `SourceExpr` 时开始依赖普通源码 AST 细节。
- 构造 `TypedExpr` 时绕过 ordinary type checker。
- 把 `ConstValue` 当作表达式回填，制造“看上去像代码，其实是值”的模糊地带。

这正是后续实现最容易任意发挥的地方。

### 3. 它们拥有不同的 authority

宏系统里最重要的不是“能表示什么”，而是“允许谁做什么”。

- `SourceExpr` 代表用户源码，provider 不应默认拥有读取任意用户表达式主体的权力。
- `AttrExpr` 代表用户显式交给宏消费的配置，因此 provider 可以看到。
- `ConstValue` 代表编译器确认可公开给宏消费的确定性编译期数据。
- `GeneratedExpr` 代表 provider 提出的候选程序片段，但并不受信任。
- `TypedExpr` 代表 type checker 已经确认过的可信内部对象。

统一 `Expr` 的直接后果是 authority 也被模糊化。宏 provider 会开始“顺手”拿到自己本不该拿到的数据和能力。

### 4. 它们依赖不同的不变量

`TypedExpr` 依赖 resolved types、call signatures、mutability、owner 信息。`SourceExpr` 和 `AttrExpr` 根本不满足这些条件。

反过来，`AttrExpr` 需要的是：

- 可稳定诊断重复 keyed args。
- 可表达 path/type ref。
- 可做受限 CTFE。
- 可保持用户 attribute span。

这些都不是 ordinary `TypedExpr` 的职责。

因此，如果试图“复用一个树”，最后一定会塞进一堆可空字段、模式不全分支、以及“这个字段只在 phase X 有意义”的隐含规则。那不是复用，而是把边界藏起来。

### 5. 统一 Expr 会让测试边界失效

如果 provider ABI 暴露的是一个巨大统一内部 `Expr`，测试很难回答这些本该明确的问题：

- provider 是否能拿到用户函数体？
- provider 是否能伪造 checked code？
- default 值到底是在 CTFE 阶段判定，还是在 ordinary typing 阶段判定？
- generated declaration 是否仍要经过 ordinary typing？
- raw source text 是否可以作为 debug artifact 存在，但不能作为 primary output？

分层对象的价值之一，就是让这些问题变成数据类型和测试契约，而不是口头约定。宏边界里的 `Expr[T = Any]` 也应该服务于这个目标：它必须是一个受限、可验证、可序列化、不能伪装成 `TypedExpr` 的代码值，而不是“随便把现有 AST 塞给 provider”。

## 推荐术语与边界

推荐把宏系统边界冻结成下面这套术语。

### 编译器内部对象

- `SourceExpr`
- `SourceType`
- `UntypedDecl`
- `TypedExpr`
- `TypedDecl`

这些对象可以在编译器内部自由演化，但不应直接作为 provider ABI。

### 宏边界对象

- `AttrExpr`
- `ConstValue`
- `DeclShape`
- `ReflectionMetadata`
- `Expr[T = Any]`
- `GeneratedDecl`
- `MacroInput`
- `MacroOutput`

这里的 `Expr[T = Any]` 是 provider API 名称。它可以由编译器内部的 `GeneratedExpr` 树实现，也可以未来换成更稳定的 macro AST；关键是 provider 不直接拿 `UntypedExpr`，也不直接产出 `TypedExpr`。

建议关系如下：

```text
UntypedDecl / SourceType
  -> DeclShape / ReflectionMetadata
  -> MacroInput
  -> MacroOutput(GeneratedDecl / Expr[Any])
  -> adapter
  -> ordinary UntypedDecl / TypedDecl path
```

### 建议的数据模型

第一版可以尽量小，不要过度设计。

#### `AttrExpr`

```text
AttrExpr =
  AttrBool
  AttrString
  AttrInt
  AttrPath
  AttrTypeRef
  AttrArray(List[AttrExpr])
  AttrRecord(List[(fieldName, AttrExpr)])
  AttrKeyedArg(name, AttrExpr)
```

要求：

- 能保留 span。
- 能稳定排序和显示。
- 不允许普通控制流、lambda、局部绑定、任意调用。

#### `ConstValue`

```text
ConstValue =
  ConstBool
  ConstString
  ConstInt
  ConstTypeRef
  ConstPathRef
  ConstArray(List[ConstValue])
  ConstRecord(List[(fieldName, ConstValue)])
  ConstTag(name, payload?)
```

要求：

- 有限。
- 可序列化。
- 可作为 provider input 与 golden 输出。
- 不依赖宿主对象 identity。

#### `DeclShape`

`DeclShape` 不是完整 AST，而是“宏需要知道的声明外形”。

建议至少包含：

- 类型名 / 模块路径 / 可见性 / span
- 字段列表
- 字段解析后类型
- 变体列表
- `@derive(...)` 与其他 macro attributes
- doc comments
- 默认值槽位

重点是：`DeclShape` 应该足够 support `cli.Parser`，但不要直接把任意 function body 暴露进去。

#### `DefaultValueInfo`

默认值是最容易引发边界混乱的地方，建议单独建模，而不是用一个 `Option[Expr]` 糊过去。

推荐第一版使用：

```text
DefaultValueInfo =
  NoDefault
  ConstDefault(ConstValue, originSpan)
  NonConstDefault(originSpan, reason)
```

这样做的原因：

- `cli.Parser` 需要 typed defaults，但它真正需要的是“可确定的默认值”，不是任意源码表达式。
- 如果 provider 直接拿到 `SourceExpr` 默认值，后续就会有人要求 provider 自己解释普通源码。
- `NonConstDefault` 可以让 compile-time 诊断更明确，而不是让 provider 猜测失败原因。

## 过程宏与 `Expr[T = Any]`

上面的分层原则不等于 Cosmo 永远没有 `Expr`。更合理的路线是：把 `Expr[T = Any]` 设计成 macro protocol 的公开代码值，而不是编译器内部所有表达式阶段的统一实现。

### 初期：`Expr[Any]` 表示 untyped code value

第一阶段可以把表达式宏的参数和输出建模为：

```text
Expr[T = Any]
```

其中默认的 `Any` 不表示对象语言里真的有一个可随意使用的 `Any` 值，而表示：

- 这是一段对象语言表达式代码。
- 它尚未经过 ordinary type checking。
- provider 可以组合、转发、splice、生成它。
- provider 不能声称它已经是 `TypedExpr`。
- 它进入用户程序前仍必须交给 ordinary typer。

换句话说：

```text
Expr[Any] != TypedExpr
Expr[Any] != runtime Any value
Expr[Any] == untyped macro code value
```

这个默认值让初期过程宏有可用 API，但不要求现在就设计完整 typed quotation。

### `vec` 的推荐模型

`vec` 与 `@derive(cli.Parser)` 不同。它不是声明宏，而是表达式位置的 ordinary-call-shaped macro：

```text
vec(a, b, c)
```

推荐输入输出如下：

```text
ExprMacroInput:
  path: PathRef
  args: List[Expr[Any]]
  callSpan: SourceSpan

ExprMacroOutput:
  expr: Expr[Any]
  diagnostics: List[MacroDiagnostic]
```

对 `vec` 来说，provider 不需要知道每个元素的类型。它只需要生成一个待检查的表达式，例如：

```text
{
  val tmp = Vec::new()
  tmp.push(<arg0>)
  tmp.push(<arg1>)
  tmp.push(<arg2>)
  tmp
}
```

这里 `<argN>` 是对输入 `Expr[Any]` 的 splice。元素类型、`Vec::new` 是否能推导、`push` 调用是否合法，都由 ordinary type checking 负责。

### 初期 `Expr[Any]` 的能力限制

为了避免表达式宏直接升级成 type checker 插件，初期 `Expr[Any]` 应该有明确限制：

- provider 可以构造表达式节点。
- provider 可以把输入表达式 splice 到输出表达式。
- provider 可以读取 literal / path / 普通调用参数这类受限表面结构。
- provider 不应默认检查任意用户表达式内部语义。
- provider 不应查询表达式类型。
- provider 不应拿到 `TypedExpr`、scope resolver、lowering context 或 backend handle。

这条路线支持 `vec`、`format`、`sql` 这类常见过程宏的早期版本：

```text
literal / const config + Expr[Any] fragments -> Expr[Any]
```

但它暂时不承诺完整的 typed AST reflection，也不开放 Rust 风格 token-tree macro。表达式宏的输入必须已经是合法 Cosmo 表达式 AST，不能借宏调用引入非 Cosmo 语法。

### 未来：`Expr[T]` 表示 typed quotation

长期可以把默认的 `Any` 收紧为泛型结果类型：

```text
Expr[T]
```

其语义应接近 staged programming 里的 `Code[T]`：

```text
Expr[T] 是一段代码；如果它被 splice 到未来阶段并通过检查，会产生 T。
Expr[T] 不是 T 本身。
```

例如：

```text
quote(1 + 2): Expr[i32]
```

这不表示现在有一个 `i32` 值，而表示现在有一段“将来产生 `i32`”的代码。

因此下面这种关系必须保持：

```text
Expr[i32] 不能隐式当作 i32 使用
Expr[i32] 只能通过 quote/splice/builder/typecheck 等显式边界参与程序构造
```

未来 `Expr[T]` 需要配套几个概念，否则类型参数只是装饰：

- `TypeRef[T]` 或 `Type[T]`，表示类型 `T` 的编译期证据。
- quote/splice 的 phase 规则。
- 防止 scope extrusion 的作用域规则。
- typed expression macro 的 re-check 或 validation 策略。
- `Expr[T]` 与 `TypedExpr` 的边界：前者是宏语言代码值，后者是 ordinary typer 产物。

可以把演进顺序写成：

```text
Expr[Any]
  -> Expr[T] with TypeRef[T]
  -> typed quotation and splice
  -> optional typed AST reflection
```

不要反过来从完整 typed AST reflection 开始。

### 参考模型

这个方向可以参考几类语言设计，但不直接照搬：

- Scala 3 的 `Expr[T]` / `Type[T]` / `Quotes`：`Expr[T]` 表示结果类型为 `T` 的 quoted code，`Type[T]` 是类型证据，`Quotes` 是宏展开上下文。
- MetaOCaml 的 code type：`. < ... > .` 构造 future-stage code，escape/splice 把 code 插回未来阶段。
- Template Haskell 的 typed quotation：typed quote 产生 `Code m a`，和 `Expr[T]` 的直觉相近。
- Rust procedural macro 的 `TokenStream -> TokenStream`：适合作为“展开后再 ordinary typecheck”的保守参考，但 Cosmo 不应停留在 raw token/string 输出。

对 Cosmo 来说，实用结论是：

```text
初期过程宏使用 Expr[Any]。
Expr[Any] 是结构化 untyped code value。
输出仍进入 ordinary typer。
未来再引入 Expr[T]，但 Expr[T] 也不能替代 TypedExpr。
```

## derive 宏与普通宏函数的语法设计

derive 宏和普通宏函数应该共享底层 `MacroProtocol`，但不应强行共享 surface syntax。它们的用户意图不同：

```text
derive macro:
  修饰一个声明，从声明反射数据生成新的声明。

普通宏函数 / expression macro:
  出现在表达式位置，从参数代码生成新的表达式代码。
```

因此推荐把语法分成两条入口：

```cosmo
@derive(cli.Parser)
class Cli:
  ...

val xs = vec(1, 2, 3)
val msg = format("{}: {}", name, count)
```

### derive 宏语法

第一版推荐保留：

```cosmo
@derive(path)
```

例如：

```cosmo
@derive(cli.Parser)
@command(name = "cosmoc", version = "0.1.0")
class Cli:
  @arg(long = "package", short = "p")
  val package: Option[Path]
```

这里有一个分工：

```text
@derive(cli.Parser)
  选择 derive provider。

@command(...) / @arg(...)
  是 provider-owned attributes，由 provider 声明消费。
```

不推荐把 provider config 都塞进 `@derive(...)`：

```cosmo
@derive(cli.Parser(name = "cosmoc", version = "0.1.0"))
```

原因是 `derive` 会同时承担 provider resolution 和 provider config 两种职责，attribute consumption、typo 诊断、span 定位都会变得更差。`@derive(path)` 应保持很薄，只表达“对这个声明运行哪个 provider”。

多个 derive 建议先使用重复 attribute：

```cosmo
@derive(cli.Parser)
@derive(json.Serializable)
class Cli:
  ...
```

这比列表形式更容易冻结 provider ordering 与每个 derive 的诊断 span。未来可以再考虑：

```cosmo
@derive(cli.Parser, json.Serializable)
```

但它不应成为第一版必须支持的语法。

derive provider 的语义签名可以写成：

```text
DeriveProvider:
  DeriveInput[ReflectClass | ReflectSum] -> MacroResult[List[GeneratedDecl]]
```

对 `cli.Parser` 来说：

```text
cli.Parser:
  DeriveInput[ReflectClass] -> MacroResult[List[GeneratedDecl]]
```

provider 看到的是声明形状、字段、字段类型、属性、默认值信息和 span；provider 返回的是 generated declarations，而不是 `TypedExpr` 或 backend hook。

### 普通宏函数语法

普通表达式宏建议采用和 Scala 类似的 ordinary expression call 形态：

```cosmo
path(...)
```

第一版只支持合法 Cosmo 表达式参数：

```cosmo
val xs = vec(1, 2, 3)
val msg = format("{}: {}", name, count)
```

这意味着普通表达式宏不是 Rust 风格 token-tree macro：

- 参数必须先被 parser 接受为 Cosmo expression。
- 宏调用不能引入非 Cosmo token、临时 DSL 语法、任意 token tree 或 parser 插件。
- `sql("...")` 可以把 SQL 写在字符串里，但不能让 `sql(select * from t)` 变成非 Cosmo 语法。
- `{ ... }` 只有在它本身是合法 Cosmo block expression 时才能作为参数，不提供任意 block DSL。

普通调用形态的代价是：macro provider resolution 会和 ordinary function resolution 发生在同一类 surface syntax 上。因此必须冻结解析规则。建议第一版采用：

```text
1. name resolution 先判断 callee 是否是 compile-time macro provider。
2. 如果 callee 是 macro provider，则进入 expression macro expansion。
3. 如果 callee 是 ordinary function，则进入 ordinary call typing。
4. 如果 callee 同时存在 macro provider 和 ordinary function，必须按导入/声明规则消歧，不能静默任选。
```

这条路线更接近 Scala 3：宏调用看起来像普通表达式，区别来自 callee 的定义与 compile-time provider identity，而不是 `!` 这类额外 token。

普通 expression macro 的第一版签名可以写成：

```text
ExprMacroProvider:
  ExprMacroInput -> MacroResult[Expr[Any]]

ExprMacroInput:
  path: PathRef
  args: List[Expr[Any]]
  callSpan: SourceSpan
```

`vec` 的 provider 不需要知道元素类型：

```text
vec:
  List[Expr[Any]] -> MacroResult[Expr[Any]]
```

它生成待检查代码，ordinary typer 再推出最终类型或报错。未来 typed macro 成熟后，可以增加更强签名：

```text
vec[T]:
  List[Expr[T]] -> MacroResult[Expr[Vec[T]]]
```

但这需要 `TypeRef[T]`、typed quote/splice、scope safety 和 re-check 规则先稳定，不应作为第一版前提。

### 普通宏定义语法

第一版 compiler-hosted provider 不需要用户写宏定义语法，只需要 provider registry：

```text
cli.Parser -> compiler-hosted derive provider
vec        -> compiler-hosted expression provider
```

未来 self-hosted 宏可以考虑显式 provider-kind 语法：

```cosmo
macro derive Parser(input: DeriveInput[ReflectClass]): MacroResult[List[GeneratedDecl]]:
  ...

macro expr vec(input: ExprMacroInput): MacroResult[Expr[Any]]:
  ...
```

或者更接近函数的形式：

```cosmo
macro def vec(args: List[Expr[Any]]): Expr[Any] =
  ...
```

但不建议做成普通 `def` 加 attribute：

```cosmo
@macro
def vec(args: List[Expr[Any]]): Expr[Any] =
  ...
```

原因是宏 provider 不是普通 runtime function。它需要 provider identity、phase、capability set、diagnostic context、fresh name allocator、determinism contract。用普通 `def` 加魔法 attribute 容易让实现者把宏运行时和普通函数运行时混在一起。

### attribute macro 暂缓

未来可能需要 attribute macro：

```cosmo
@test
def parses_package_flag(): ...

@route(method = "GET", path = "/packages")
def handle(...): ...
```

但第一版不建议开放任意 attribute macro。当前应只支持：

```text
@derive(path)
provider-owned attributes such as @command / @arg
```

否则未知 attribute 到底是 typo、metadata、derive-owned config，还是 attribute macro，会很难稳定诊断。第一版应坚持：未被 provider 声明消费的 attribute 必须报错。

### 与 Scala 3 的关系

Scala 3 的普通宏是：

```text
inline def surface(...) = ${ impl(...) }
impl: Expr[A] -> Expr[B]
```

它的 derive 是：

```text
case class Person(...) derives Show
```

并通过 compiler-generated `Mirror` 和 type class 的 `derived` 方法生成 given instance。Scala 的设计给 Cosmo 的启发是：

- `Expr[T]` 应该表示 typed code value，而不是 `T` 本身。
- 类型信息要通过 `Type[T]` / `TypeRef[T]` 这类显式证据传入。
- 宏展开上下文应像 `Quotes` 一样被能力化，而不是暴露整个 compiler。
- derive 需要结构化 reflection metadata，不应让 provider 直接 inspect compiler internals。

Cosmo 在 ordinary expression macro 的 surface syntax 上可以采用 Scala 风格：宏调用看起来像普通函数调用，区别来自 callee 是否解析为 compile-time provider。Cosmo 仍不必照搬 Scala 的完整 typed-first 宏系统，初期更稳的选择是：

```text
@derive(path) for declaration derive
path(...) for expression macro
Expr[Any] first, Expr[T] later
```

## 两套设计备选

### 方案 A：严格分层，宏边界只暴露专用对象

这是推荐方案。

做法：

- 保留 `SourceExpr / TypedExpr` 作为编译器内部对象。
- 引入 `AttrExpr / ConstValue / DeclShape / ReflectionMetadata / Expr[Any] / GeneratedDecl` 作为宏边界对象。
- 宏 provider 只接收 `MacroInput`，只返回 `MacroOutput`。
- generated declarations 之后进入 ordinary type checking。

优点：

- 相位边界清楚。
- provider authority 最小。
- 可以先做 compiler-hosted provider，以后再换自托管或 Wasm runner。
- 生成代码和手写代码走同一条 typing/lowering/backend 路径。
- golden/negative test 契约容易冻结。

代价：

- 需要做几层 adapter。
- 初期会觉得“为什么不直接复用现有 AST”。

我认为这个代价是必要的，因为它换来的是可控性。

### 方案 B：统一所有阶段的大树加 phase tag

这是不推荐方案。

做法：

- 定义一个大一统 `MacroExpr` 或直接复用 `UntypedExpr`，并试图同时表达 source、attribute、const、generated、typed 等阶段。
- 用 tag / metadata 区分 attribute、default、generated、checked 等阶段。

优点：

- 短期实现快。
- 复用现有 pretty printer / traversal 看上去更省事。

问题：

- provider 很快会依赖编译器内部树形状。
- boundary 会退化成“约定俗成别乱用这些分支”。
- `TypedExpr` 注入、raw source fallback、任意 `SourceExpr` 暴露都会变得很难彻底禁止。
- 执行后端和语义层会被 AST 细节绑死。

如果目标是“先跑起来”，方案 B 很诱人；如果目标是“后面 agent 不要随便实现成另一套语言”，方案 B 基本不可接受。

## 推荐的 phase boundary

结合当前 `Pipeline` 和 `MlttTyper`，最小可落地的 phase 可以这样切：

```text
1. Parse
2. Elaborate
3. Preserve accepted macro attributes
4. Declaration shape / signature collection
5. ConstEval for attributes and admitted defaults
6. Declaration macro expansion
7. Expression macro expansion to `Expr[Any]`
8. Ordinary type checking
9. Lowering / backend
```

### 各阶段职责

#### 1. Parse

- 只负责把语法读进来。
- 不做宏执行。

#### 2. Elaborate

- 把 parser 语法收窄到 cosmo0 的 elaborated 形式。
- 当前已有 `@include` / `@extern` 特例逻辑继续保留。

#### 3. Preserve accepted macro attributes

- 不执行宏，只保留结构化 attribute 数据。
- 只接受允许挂载在声明形状上的 macro attribute。
- 其他 unsupported decorator 仍应报错。

这是当前 `Elaborator` 最小但关键的变化点。

#### 4. Declaration shape / signature collection

- 解析字段类型、方法签名、变体形状、别名展开后的声明级信息。
- 不检查普通函数体。
- 不解释 generated code。

这一步与现有 `MlttTyper` 的 collect pass 方向一致，说明不是凭空增加复杂度，而是把已经存在的编译器结构外显出来。

#### 5. ConstEval for attributes and admitted defaults

- 把 `AttrExpr` 降成 `ConstValue`。
- 对字段默认值做“是否可作为 compile-time const 暴露”的判定。
- 不运行 target program。
- 不依赖 backend。

#### 6. Declaration macro expansion

- 输入：`MacroInput`
- 输出：`MacroOutput`
- 内容：反射元数据、候选 attribute、const defaults、provider config
- 不给 provider 完整 compiler state
- 不给 provider `TypedExpr`

#### 7. Expression macro expansion to `Expr[Any]`

- 输入：表达式位置的 macro call 与参数 `Expr[Any]`。
- 输出：新的 `Expr[Any]`。
- 不读取参数的 ordinary type。
- 不直接生成 `TypedExpr`。
- 输出表达式继续进入 ordinary expression typing。

这一阶段可以作为 `vec(...)` 的落点。它和 declaration macro 的共同点是都输出待检查代码；差异是 declaration macro 产出 declarations，expression macro 产出 expression。

#### 8. Ordinary type checking

- 统一检查用户源码与 generated declarations。
- generated expressions 仍走 ordinary typer。
- provider 生成错误代码时，诊断需要同时指向 generated span 与 origin span。

#### 9. Lowering / backend

- 只看到普通 checked program。
- 不应再看到 macro reflection tables、macro attrs、provider runtime、或 CTFE 内部对象。

### 这个 phase 切法对当前实现的意义

这条路线尽量复用当前实现：

- `Pipeline` 已经在包级合并 `UntypedDecl` 后喂给 `MlttTyper`。
- `MlttTyper` 已经有 declaration collection，再做 expression typing。
- 因此最小实现可以在“合并后的 untyped declarations”附近插入一个 `DeclShape -> MacroExpand -> GeneratedDecl -> back to untyped declarations` 的阶段。

也就是说，第一版不需要把整个 compiler 推倒重来。

## compile-time computation 与 ordinary typing 的边界

这个问题里最容易失控的是：为了支持 typed defaults 或 provider 自托管，逐步把普通程序执行偷渡进 compile time。

我建议明确分成两个服务。

### `ConstEval`

职责：

- 只处理 `AttrExpr` 与允许暴露的默认值。
- 产出 `ConstValue` 或 deterministic diagnostic。
- 使用有限值域与有限 capability。

不负责：

- 调 ordinary function body。
- 执行 target program。
- 依赖 backend 生成代码。
- 访问宿主文件系统、网络、环境变量、时间、随机数。

### `ProviderEval`

职责：

- 执行 provider 本身的宏逻辑。
- 输入为 `MacroInput`。
- 输出为 `MacroOutput`。

不负责：

- 绕过 ordinary typer。
- 直接构造 `TypedExpr`。
- 修改编译器全局状态。

### 属性参数

属性参数的推荐处理方式：

- provider 应该拿到结构化 `AttrExpr`。
- 编译器可以提供一个“把 admitted `AttrExpr` 降为 `ConstValue`”的标准步骤。
- provider 不应自己拿到普通 `SourceExpr` 后随意解释。

原因很简单：`@arg(long = "package")` 与 `@command(version = "0.1.0")` 这种配置，本质上是声明式数据，不应变成任意 compile-time 脚本入口。

### 默认值

默认值比属性参数更微妙。

当前 cosmo0 里，字段 initializer 和参数 default 都是在 ordinary typing 期间根据期望类型去检查的。对 derived CLI 来说，真正需要的是：

- 字段是否有默认值。
- 这个默认值是否能被稳定地当作 compile-time const 公开给 provider。
- 如果不能，应该在什么阶段、以什么诊断失败。

因此我推荐：

- v1 provider input 不直接暴露任意 `SourceExpr` 默认值。
- v1 只暴露 `ConstDefault`。
- 如果用户给了 default，但它不是 admitted const，就形成 deterministic compile-time diagnostic，而不是把解释责任丢给 provider。

这可以同时保护两条边界：

- 不让 provider 变成普通表达式解释器。
- 不让 ordinary typer 被宏需求倒逼成 compile-time evaluator。

### derive provider 输入

建议第一版 `MacroInput` 至少包含：

- provider path / resolved provider identity
- target `DeclShape`
- `ReflectionMetadata`
- candidate attributes
- admitted `ConstValue`
- default value info
- source spans
- provider config

明确不包含：

- 任意用户函数体
- `TypedExpr`
- `MlttTyper` 实例
- lowering context
- backend handles
- ambient host state

### derive provider 输出

建议第一版 `MacroOutput` 只包含：

- `GeneratedDecl`
- `consumedAttributes`
- provider-authored diagnostics
- optional generated-source summary

明确不允许：

- raw generated source 作为 primary output
- `TypedExpr`
- 直接 patch `TypedModule`
- 直接 patch lowering IR

### expression macro 输入

建议第一版 expression macro 输入只包含：

- macro path / resolved provider identity
- call span
- 参数 `Expr[Any]`
- admitted literal / const config

明确不包含：

- 参数 ordinary type
- `TypedExpr`
- 当前 scope resolver 的可变 handle
- lowering context
- backend handles

### expression macro 输出

建议第一版 expression macro 输出只包含：

- 一个 `Expr[Any]`
- provider-authored diagnostics
- optional expansion summary

这个输出必须进入 ordinary expression typing。provider 不能把 `Expr[Any]` 标记成已经 checked，也不能把它直接塞进 lowering。

## 语义层与执行后端层

这一节要避免一个常见混淆：执行容器不是语言语义。

### 我推荐的分层

```text
Language semantics:
  MacroProtocol
  Reflection schema
  AttrExpr / ConstValue model
  Expr[Any] / GeneratedDecl / hygiene / diagnostics contract
  Capability model
  Determinism contract

Execution backend:
  compiler-hosted provider
  internal Macro IR interpreter
  Wasm sandbox runner
  Wasm Component runner
  debug adapter / summary printer
```

换句话说：

- “宏能看到什么、能产出什么、什么算非法、什么必须 deterministic”属于语义层。
- “宏具体跑在 Scala、Cosmo 自解释器、Wasm module、还是 Wasm component”属于执行后端层。

### 对几种执行模型的评估

| 方案 | 适合作为语义层 | 适合作为执行后端 | 结论 |
| --- | --- | --- | --- |
| 内部 Macro IR 解释器 | 可以作为参考语义与 MVP 实现 | 适合 | 推荐作为第一长期主线 |
| Wasm sandbox | 不适合作为语言语义本身 | 适合 | 推荐作为长期执行容器 |
| Component Model | 不适合作为表达式语义本身 | 适合 | 推荐作为长期 provider ABI / 分发层 |
| dynamic linking | 不适合 | 只适合作为受限实现细节 | 不推荐成为默认长期模型 |

### 内部 Macro IR 解释器

优点：

- 最容易严格控制 capability。
- 可以把 compile-time determinism、fuel、allocation budget 做成编译器内建规则。
- 与当前 compiler-hosted provider 的语义距离最近。
- 最适合作为 spec reference implementation。

风险：

- 多语言 provider 生态较弱。
- provider packaging 需要额外设计。

推荐结论：

- 让 `MacroIR` 成为宏计算的核心语义对象。
- 解释器是 `MacroIR` 的第一参考实现和第一执行后端。

### Wasm sandbox

优点：

- 隔离边界清晰。
- capability 控制天然适合 compile-time execution。
- 多语言 provider 路线更现实。

风险：

- 不能让 Wasm 自身的内存模型、ABI 细节决定 Cosmo 宏语义。
- debug、span、类型引用身份传递需要额外设计。

推荐结论：

- 适合作为长期执行容器。
- 不适合直接成为“宏语义 = Wasm 语义”。

### Component Model

优点：

- 它更像“显式接口与值交换”的 ABI 层，而不是共享地址空间插件。
- 很适合承载 `MacroInput -> MacroOutput` 这样的协议。
- 对多语言 provider 与 provider packaging 更友好。

风险：

- 仍属于执行和分发层，不应倒过来定义语言内部 `Expr` / `Type` 语义。
- 稳定 identity、span、type ref 如何跨组件传输，需要单独设计。

推荐结论：

- 适合作为长期 provider ABI 和打包模型。
- 不适合作为语言语义的唯一来源。

### dynamic linking

这里的典型诱惑是：把 provider 做成共享库或可动态拼接模块，好像最“插件化”。

问题在于，动态链接天然容易带入这些不想要的性质：

- 宿主环境依赖
- 初始化顺序差异
- 共享全局状态
- 版本和查找路径差异
- 结果依赖加载顺序或宿主机器

这些性质与 deterministic macro expansion 的目标是冲突的。

推荐结论：

- 可以作为实现细节存在。
- 不应成为 Cosmo 宏系统的默认长期语义模型。
- 如果真的需要组合多个 provider，更适合做静态 composition 或显式 component composition。

## 如何避免后续实现任意发挥

这部分我认为比“选 Wasm 还是 IR”更重要。因为如果边界不冻结，执行后端选什么都没意义。

### 需要先冻结的中间产物

建议至少冻结下面几类 schema：

- `AttrExpr`
- `ConstValue`
- `DeclShape`
- `ReflectionMetadata`
- `DefaultValueInfo`
- `Expr[Any]`
- `GeneratedDecl`
- `MacroInput`
- `MacroOutput`
- generated-source summary schema

冻结的意思不是“以后永远不能加字段”，而是：

- 新字段需要向后兼容策略。
- provider ABI 不允许直接偷用编译器内部对象代替。
- golden tests 以 schema 和稳定显示为准。

### 需要先冻结的 phase 规则

- expansion 必须发生在 ordinary body typing 之前。
- expression macro 必须在对应表达式被 ordinary type checking 之前展开成 `Expr[Any]`。
- generated code 必须进入 ordinary typing。
- lowering 不能看见 reflection-only 数据。
- unconsumed attributes 必须在 expansion 后报错，不得漂移到 runtime。
- `@derive(path)` 只解析 provider identity，不承载 provider-specific config。
- `path(...)` 先按普通 Cosmo 表达式解析；当 callee 解析为 compile-time provider 时进入 expression macro expansion，否则进入 ordinary call typing。
- expression macro 输入不得包含非 Cosmo token tree 或 parser 插件语法。

### 需要先冻结的禁止项

下面这些最好在设计讨论稿阶段就写死，不要留给实现者“自由发挥”。

- provider 不得构造 `TypedExpr`
- provider 不得把 `Expr[Any]` 标记成 already typed
- expression provider 不得查询参数 ordinary type
- provider 不得返回 raw source text 作为 primary output
- provider 不得执行 target runtime code
- provider 不得依赖 backend availability
- provider 不得访问 ambient filesystem / network / env / time / randomness
- provider 不得默认读取任意用户 function body
- expansion 不得直接 patch lowering IR
- 未解析到 compile-time provider 的 `path(...)` 只能作为 ordinary call 继续解析；如果调用点被要求必须是宏展开，则必须报 unresolved provider。
- expression macro provider 不得接收非 Cosmo 语法片段。
- 未被声明消费的 `@arg` / `@command` / provider-owned attribute 不得静默保留。

### 需要先冻结的 hygiene 规则

- public generated declarations 必须显式声明名字与可见性
- private helper 必须使用稳定 fresh internal name
- public generated name 与 user declaration 冲突时必须 deterministic 报错
- generated diagnostics 必须同时保留 generated span 与 origin span

### 需要先冻结的测试契约

建议从一开始就把以下内容做成 golden 或 negative tests：

- reflection metadata display
- `ConstValue` display / serialization
- `Expr[Any]` display / serialization
- `GeneratedDecl` display / serialization
- expression macro expansion display
- expansion summary deterministic ordering
- repeated expansion stable
- unresolved provider path
- invalid provider output
- raw source primary output rejection
- typed expression injection rejection
- unconsumed attribute rejection
- non-const default rejection
- capability violation rejection
- public generated name collision rejection
- macro call unresolved provider rejection
- macro-required call does not silently fallback to runtime function
- non-Cosmo token-tree macro syntax rejected
- derive config typo reported as unconsumed attribute

这些测试的意义不是“证明实现存在”，而是“限制实现空间”。

## 一个面向当前代码库的最小落地版本

如果目标是“尽量小改动地把 CLI derive 跑通，同时不把长期边界做坏”，我建议第一版按下面顺序落地：

1. 在 elaboration 后保留 accepted macro attributes，不执行宏。
2. 从当前 `UntypedDecl + SourceType` collection 中抽出 `DeclShape / ReflectionMetadata`。
3. 只支持受限 `AttrExpr` 与小型 `ConstValue`。
4. 支持 class / sum derive，以及受限 expression macro。
5. expression macro 的公共表达式类型先落为 `Expr[T = Any]`。
6. declaration provider 只返回 `GeneratedDecl` 树。
7. expression provider 只返回 `Expr[Any]`。
8. 通过 adapter 把 `GeneratedDecl` 和 `Expr[Any]` 重新接入 ordinary typing 路径。
9. CLI derive 默认值只接受可形成 `ConstDefault` 的 initializer。
10. surface syntax 先冻结为 `@derive(path)` 和普通 Cosmo 调用形态 `path(...)`。
11. 暂缓任意 attribute macro 与 item macro。

这样做的好处是：

- 能满足 `cli.Parser` 的 forcing function。
- 不需要先做完整自托管宏运行时。
- 仍然把长期边界摆正了。

## 推荐结论

如果现在就要拍板，我推荐的设计结论如下：

1. 采用方案 A：严格分层，`SourceExpr / AttrExpr / ConstValue / GeneratedExpr / TypedExpr` 明确分离。
2. 宏系统的第一稳定边界不是 `UntypedExpr`，而是 `DeclShape / ReflectionMetadata / ConstValue / Expr[Any] / GeneratedDecl`。
3. compile-time computation 拆成 `ConstEval` 与 `ProviderEval`，ordinary typer 不承担宏解释器职责。
4. phase 顺序固定为“声明形状已知 -> const/default 判定 -> declaration macro expansion -> expression macro expansion -> ordinary typing -> lowering”。
5. 初期过程宏使用 `Expr[T = Any]`，未来在 quote/splice、`TypeRef[T]`、scope safety 规则清楚后再收紧到 `Expr[T]`。
6. surface syntax 第一版采用 `@derive(path)` 与 Scala 风格普通表达式调用 `path(...)`；表达式宏不允许非 Cosmo token-tree/DSL 语法；暂缓 arbitrary attribute macro、item macro、普通 `def` 加 `@macro` 的定义形式。
7. 长期语义层由 `MacroProtocol + MacroIR + capability model` 定义；Wasm sandbox、Component Model 与 native JIT 只作为执行/分发后端。
8. `cosmo-jit-sys` 可以作为可选 native support backend，以 N-API 封装 `clang::Interpreter`，但不应成为宏语义本身。
9. dynamic linking 不应成为宏系统的默认长期执行模型。
10. 要从第一版开始冻结禁止项、hygiene 规则与 golden/negative tests，否则实现会快速发散。

## 仍待主线程决定的开放问题

下面这些问题仍需要主线程拍板，文档暂不替 spec 做最终决策：

- macro expansion 放在“模块合并前”还是“模块合并后”更合适；两者对 provider 可见模块边界不同。
- `DefaultValueInfo` 是否只暴露 `ConstDefault`，还是要保留某种受限 `SourceExpr` 视图。
- `GeneratedDecl` 是否独立于 `UntypedDecl`，还是第一版先复用 `UntypedDecl` 再逐步抽离。
- `Expr[Any]` 是否允许 provider inspect 完整语法树，还是第一版只允许有限 inspection 加 splice。
- `Expr[T]` 的类型参数应如何绑定 `TypeRef[T]`，以及是否需要独立的 `Quotes` / expansion context。
- ordinary call 中 macro provider 与 ordinary function 同名时的消歧规则是否由 import priority、显式 macro import，还是 provider namespace 决定。
- 是否需要专门的语法标记表达“这里必须是 macro call”，以避免 `path(...)` 在 provider 缺失时退回 ordinary call。
- 多个 `@derive(...)` 的执行顺序是否按源码顺序冻结，或要求 provider 显式声明 ordering。
- self-hosted macro 定义语法应采用 `macro derive` / `macro expr`，还是 `macro def`。
- `AttrExpr` 的 path / type ref 在 expansion 前解析到什么程度才最稳妥。
- provider registry 与 package metadata 的绑定方式：`macroDependencies`、普通 dependencies + metadata，还是 stage/profile registry。
- generated-source summary 是冻结文本格式，还是冻结结构化 schema。
- 将来 `MacroIR` 与 Wasm component 的衔接应通过什么稳定 ABI 表达 span、type ref、path ref。

## 对 OpenSpec 的建议

如果后续要把这里的讨论沉淀回 OpenSpec，我建议优先写成 design-level 结论，而不是直接写具体实现：

- 先沉淀术语与 phase boundary。
- 再沉淀 `AttrExpr / ConstValue / Expr[Any] / GeneratedDecl` 的禁止项。
- 然后再决定是否把 `DefaultValueInfo` 与 `MacroIR` 前置进 spec。
- `cosmo-jit-sys` 应先作为实验性 native support capability 记录，等 N-API / `clang::Interpreter` smoke path 跑通后再决定是否进入正式 OpenSpec。

也就是说，先冻结“边界和禁区”，再冻结“执行容器和细节”。

## `cosmo-jit-sys` 计划：N-API + `clang::Interpreter`

本节记录当前讨论形成的 native JIT 计划。它不是替代前面 `MacroProtocol / MacroIR / GeneratedDecl` 的宏语义，而是给 C/C++ 依赖、模板实例化、native wrapper 生成提供一个可用的执行后端。

### 目标定位

`cosmo-jit-sys` 的定位建议如下：

- 它是一个 Node-API native addon，供当前 Scala.js / Node 形态的 `cosmo0` 直接加载。
- 它内部封装 Clang-Repl 的 `clang::Interpreter`，负责增量解析、编译、执行 C++ wrapper snippet。
- 它主要解决 C++ headers、template instantiation、`std::vector<T>` 这类 native interop 问题。
- 它不定义 Cosmo macro semantics，不暴露 Clang AST / LLVM IR / C++ object layout 给普通 provider。
- 它对 Cosmo 编译器可见的结果应是结构化 diagnostics、受控 symbol token、native support binding 信息；宏可见效果仍然要通过 `MacroOutput / GeneratedDecl` 回到 ordinary typing。

因此推荐分层是：

```text
Cosmo macro semantics:
  MacroProtocol
  ReflectionMetadata
  ConstValue
  Expr[Any]
  GeneratedDecl

Native JIT support:
  @cosmo/jit-sys N-API addon
  clang::Interpreter session
  generated extern "C" wrappers
  native diagnostics
  capability-checked include/library access
```

关键原则：

```text
clang::Interpreter is an implementation backend.
GeneratedDecl remains the only compiler-visible way to add Cosmo declarations.
Native symbols may support generated code, but they do not mutate Cosmo scopes.
```

### 为什么选 `clang::Interpreter`

直接使用 LLVM ORC JIT 的问题是：它处理的是 LLVM IR 与 symbol materialization，不能替我们完成 C++ name lookup、header parsing、template instantiation、overload resolution、stdlib ABI 等工作。

`clang::Interpreter` 更贴近当前需求：

- 可以增量 `#include` headers。
- 可以让 Clang 按 C++ 规则实例化 `std::vector<T>`、`std::optional<T>`、CLI11 这类模板。
- 可以生成 `extern "C"` wrapper symbol，再由 addon 查询和登记。
- 可以复用 Clang diagnostics，而不是自己解释 C++ template error。

这使得它适合作为 `cosmo-jit-sys` 的底层，而不是让 Cosmo 自己理解 C++ 模板。

### 总体架构

推荐第一版采用 in-process N-API addon，后续增加 isolated mode。

```text
cosmo0 Scala.js / Node
  imports @cosmo/jit-sys
  calls createSession / eval / loadLibrary / lookupSymbol

@cosmo/jit-sys
  N-API C++ wrapper
  owns clang::Interpreter
  owns symbol registry
  returns structured diagnostics and tokens

clang::Interpreter
  parses C++ snippets
  instantiates templates
  JIT-compiles wrapper code
  executes static initialization required by the snippet
```

长期可以拆成两种运行模式：

```text
trusted mode:
  cosmo0 directly loads @cosmo/jit-sys in the Node process.
  Used for local experiments and trusted built-in support packages.

isolated mode:
  a worker process loads @cosmo/jit-sys.
  cosmo0 communicates over IPC.
  Used for third-party providers, CI, and reproducibility-sensitive builds.
```

第一阶段可以先做 trusted mode，因为它最符合当前 `cosmo0` 的 Node/Scala.js 形态；但文档和 API 不应把 in-process native execution 当作长期唯一安全边界。

### N-API 表面 API 草案

JS/Scala.js 侧建议只看到 session-oriented API：

```ts
export type JitConfig = {
  targetTriple?: string
  resourceDir?: string
  sysroot?: string
  cxxStandard?: "c++20" | "c++23"
  includeDirs?: string[]
  libraryDirs?: string[]
  allowedLibraries?: string[]
}

export type JitDiagnostic = {
  severity: "error" | "warning" | "note"
  message: string
  file?: string
  line?: number
  column?: number
}

export type JitSymbol = {
  name: string
  token: string
  abi: "extern-c"
}

export type JitResult = {
  ok: boolean
  diagnostics: JitDiagnostic[]
  symbols?: JitSymbol[]
}

export function createSession(config: JitConfig): JitSession

export interface JitSession {
  eval(code: string, expectedSymbols?: string[]): JitResult
  loadLibrary(path: string): JitResult
  hasSymbol(name: string): boolean
  dispose(): void
}
```

第一版不建议把 native function pointer 作为 JS number 暴露。跨 JS 边界时应使用 token：

```text
symbol name -> native address -> opaque token
```

如果后续需要调用 JIT 出来的 native function，应在 addon 内部提供受控调用 wrapper，或者由 generated C++ wrapper 再挂到稳定 support boundary。不要让 Scala.js 直接操作裸指针。

### C++ addon 内部边界

N-API C++ 层可以大致这样组织：

```text
JitSession ObjectWrap
  unique_ptr<clang::Interpreter>
  symbol name -> address
  token -> address
  diagnostics buffer

methods:
  eval(code, expectedSymbols)
  loadLibrary(path)
  hasSymbol(name)
  dispose()
```

重要边界：

- `clang::Interpreter*` 不跨出 addon。
- `llvm::Expected` / `llvm::Error` 不跨出 addon。
- Clang AST / LLVM IR 不跨出 addon。
- C++ object pointer 不直接跨出 addon，除非它被包成受控 opaque handle。
- 对外只返回 JSON-like diagnostics、symbol token、以及 wrapper metadata。

### C++ 模板实例化模型

`cosmo-jit-sys` 不让 Cosmo 直接链接 `std::vector<T>` 方法，而是生成 wrapper snippet，让 Clang 负责实例化模板。

例如：

```cpp
#include <vector>
#include <string>

using CosmoVecString = std::vector<std::string>;

extern "C" CosmoVecString* cosmo_vec_string_new() {
  return new CosmoVecString();
}

extern "C" void cosmo_vec_string_push(
    CosmoVecString* vec,
    const char* value) {
  vec->push_back(std::string(value));
}

extern "C" size_t cosmo_vec_string_len(CosmoVecString* vec) {
  return vec->size();
}
```

对 Cosmo 来说，跨边界的不是 `std::vector<std::string>` 的 C++ ABI，而是：

```text
opaque native handle
extern "C" wrapper symbols
explicit ownership rules
```

这条路线同样适用于 CLI11：

```text
CLI11 C++ API / templates / exceptions
  stay inside generated wrapper or cli11-sys boundary

Cosmo-facing API
  stable functions
  opaque handles
  structured parse results
  generated Cosmo declarations
```

### Capability 与安全约束

因为 `clang::Interpreter` 是 native execution，默认能力必须收紧。建议第一版就把约束写进接口层，而不是等接入 provider 后再补。

允许项：

- include manifest 声明的 include dirs。
- load manifest 声明的 library paths。
- compile generated wrapper snippets。
- expose expected `extern "C"` symbols。
- return structured diagnostics。

默认禁止项：

- 任意 host include path。
- 任意 `LoadDynamicLibrary` / `dlopen`。
- 任意 filesystem / network / environment access 作为 provider observable effect。
- process spawn。
- 返回 raw source 作为宏 primary output。
- JIT code 直接修改 Cosmo compiler state。
- JIT code 直接制造 `TypedExpr` 或 patch `TypedModule`。

trusted mode 可以先不做完整 OS sandbox，但 API 仍应携带 capability set。isolated mode 再把 capability 落到 worker process、restricted cwd、resource limits、library allowlist 等具体机制。

### 与宏系统的交互

`cosmo-jit-sys` 与 macro expansion 的推荐关系：

```text
Macro provider:
  receives ReflectionMetadata / ConstValue
  optionally asks cosmo-jit-sys to prepare native wrappers
  receives symbol tokens / support binding metadata
  emits GeneratedDecl calling stable support boundary

Compiler:
  validates GeneratedDecl
  integrates generated declarations
  runs ordinary type checking
  lowers generated calls normally
```

不推荐的关系：

```text
Macro provider:
  passes compiler internals to native JIT
  lets native code mutate name resolution tables
  treats JIT symbol registration as Cosmo declaration insertion
  bypasses GeneratedDecl validation
```

换句话说，`cosmo-jit-sys` 可以帮助 provider 计算和准备 native support，但不能成为另一个 compiler plugin API。

### Package 与目录建议

初期可以按 workspace package 放置：

```text
packages/cosmo-jit-sys/
  package.json
  index.js
  index.d.ts
  binding.gyp or CMakeLists.txt
  src/addon.cpp
  src/jit_session.cpp
  src/jit_session.hpp
```

如果后续 native 代码增长，可以再拆：

```text
native/cosmo-jit-sys/
  CMake project
  C++ implementation

packages/cosmo-jit-sys/
  Node package
  N-API binding loader
  TypeScript declarations
  prebuild metadata
```

Clang/LLVM 依赖较重，长期需要考虑 prebuild：

```text
prebuilds/linux-x64
prebuilds/linux-arm64
prebuilds/darwin-arm64
prebuilds/darwin-x64
```

第一版实验可以要求本机安装匹配版本的 LLVM/Clang，并通过 `llvm-config` 或 CMake package discovery 找到依赖；发布前再决定是否 vendor 或分发 prebuilt addon。

### 落地阶段

建议按下面顺序落地，避免一开始就把它绑进宏系统核心路径。

1. `cosmo-jit-sys` native addon skeleton。
   - Node 能 load addon。
   - `createSession` / `dispose` 可用。

2. 最小 C++ eval。
   - `eval("extern \"C\" int add(int a, int b) { return a + b; }")` 成功。
   - 可以 lookup expected symbol，并返回 token。
   - Clang diagnostics 转为 JS object。

3. Template smoke test。
   - `#include <vector>`。
   - 实例化 `std::vector<int>` 或 `std::vector<std::string>` wrapper。
   - 验证 wrapper symbol 可注册。

4. Manifest 与 capability gate。
   - session config 携带 include dirs、library dirs、allowed libraries。
   - `loadLibrary` 只接受 allowlist。
   - diagnostics 中记录 rejected capability。

5. Scala.js facade。
   - 在 `cosmo0` 侧加最小 JS import facade。
   - 不接入 macro expansion，只做 isolated fixture 或 smoke command。

6. Support binding metadata。
   - 定义 JIT 出来的 wrapper 如何被记录为 support binding。
   - 明确 symbol token、native artifact hash、snippet hash、Clang version、target triple。

7. Macro provider integration experiment。
   - 选择一个小 provider 或 fixture。
   - provider 通过 `cosmo-jit-sys` 生成 native wrapper。
   - provider 仍只输出 `GeneratedDecl`。

8. Isolated mode。
   - 把同一 addon 放进 worker process。
   - 主 `cosmo0` 通过 IPC 调用。
   - 为第三方 provider 或 CI 提供默认安全路径。

9. OpenSpec 沉淀。
   - 如果实验通过，再把 `cosmo-jit-sys` 写成可选 native support capability。
   - 不把它写成 macro semantics 的必要条件。

### 第一版验收标准

第一版不以“能跑任意 C++”为成功标准，而以边界清楚为标准。

最低验收：

- Node 可以加载 addon。
- session 生命周期可控。
- C++ snippet diagnostics 可结构化返回。
- `extern "C"` symbol 可 lookup。
- `std::vector<T>` wrapper 可通过 Clang 实例化。
- addon 不暴露 Clang/LLVM C++ object 给 Scala.js。
- 宏系统没有因为 addon 存在而绕过 `GeneratedDecl`。

负面验收：

- 未 allowlist 的 library load 被拒绝。
- 未 allowlist 的 include path 被拒绝或至少在 trusted mode 外被拒绝。
- provider 不能把 raw C++ snippet 当作 Cosmo generated source。
- provider 不能把 native symbol registration 当作 Cosmo source declaration。

### 仍需单独决策的问题

`cosmo-jit-sys` 本身还有几项需要后续拍板：

- 第一版是否允许 JS 侧调用 JIT symbol，还是只允许注册 wrapper metadata。
- symbol token 如何跨 session、cache、worker process 表达。
- Clang/LLVM 版本如何 pin，以及是否允许系统 LLVM。
- addon 构建使用 `node-gyp`、`cmake-js`，还是仓库现有 CMake/native pipeline。
- generated wrapper snippet 的 cache key 是否包含 Clang version、target triple、include dirs、library hashes、compiler flags。
- `cosmo-jit-sys` 与 `cli11-sys` 的关系：CLI11 是固定 support package，还是由 `cosmo-jit-sys` 按需生成 wrapper。
- trusted in-process mode 是否仅用于本地开发和官方 provider。
- isolated mode 的 IPC schema 是否复用 `MacroInput / MacroOutput` 的 serialization 层。

推荐默认答案：

```text
Start with trusted in-process N-API for experiments.
Keep API token-based and diagnostics-structured.
Do not expose raw pointers to Scala.js.
Do not connect it to macro semantics until GeneratedDecl boundary is stable.
Add isolated worker mode before supporting third-party providers.
```
