# The Plasm Compiler

Experimental, compiled, strongly-typed, functional, system programming language that inherits memory regions and algebraic effects ideas, focused on dev experience, extensibility and reliability.

Mainly inspired by [Rust](https://github.com/rust-lang/rust)'s type system, [Verona](https://github.com/microsoft/verona)'s and [Pony](https://github.com/ponylang/ponyc)'s memory managment systems, [Python](https://github.com/python/cpython)'s simplicity, [Koka](https://github.com/koka-lang/koka)'s effect system, and [Zig](https://github.com/ziglang/zig)'s comptime.

[![Discord](https://img.shields.io/badge/Discord-Server-5865F2?style=for-the-badge&logo=discord&logoColor=white)](https://discord.gg/FwBDKTAjnw)

### Core Design Principles

* **Extensibility** - ...
* **Transparency** - ...
* **Readability** - ...
* **One-Way Designability** - ...
* **Output Performace & Stability > Compilation Time** - ...

## Quick Start

For cli help reference:

```shell
cargo run --help
```

For example, following command will parse `test.sm` file, analyse, stop on HIR stage and emit the result in a text format:

```shell
cargo run emit test.sm --stage hir --format text
```

To use cli directly without Cargo execute:

```shell
cargo install --path cli
```

And then you can write just `plasm --help`.

## Status

### Early Stage Roadmap (MVP)

This is the roadmap for implementing a minimal working framework that will serve as the base for the proposed innovative features. Basically, this is the way to create a "Rustic C" compiler.

- [x] Lexer & tokens stream
- [x] AST definition
- [x] AST parser
- [x] HIR/OptHIR/THIR definitions
- [x] AST to OptHIR translator
- [x] OptHIR to THIR translator (type inference)
- [x] MIR definition
- [ ] THIR to MIR lowering
- [ ] MIR to LLVM-IR lowering
- [ ] LLVM-IR to object file compilation
- [ ] Object files linking

### Features Status Table

<table>
  <thead>
    <tr>
      <th>Feature</th>
      <th>Lexer</th>
      <th>AST</th>
      <th>HIR</th>
      <th>MIR</th>
      <th>LLVM-IR</th>
      <th>Example</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th colspan="7" align="center">Variables & Functions</th>
    </tr>
    <tr>
      <th>Local variables</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td><code>let x = 1</code></td>
    </tr>
    <tr>
      <th>Function Defenition</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td><code>fn func() {}</code></td>
    </tr>
    <tr>
      <th>Input Args</th>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>fn func(arg: i32) {}</code></td>
    </tr>
    <tr>
      <th>Output Args</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>fn func() -> i32 {}</code></td>
    </tr>
    <tr>
      <th>Function Calls</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td><code>func()</code></td>
    </tr>
    <tr>
      <th>Args Passing</th>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>func(var1, var2)</code></td>
    </tr>
    <tr>
      <th>Named Args</th>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>func(arg2=var2, arg1=var2)</code></td>
    </tr>
    <tr>
      <th>Global Variables</th>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>let SOME_CONSTANT: i32 = 42</code></td>
    </tr>
    <tr>
      <th>String Literals</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>let a = "Hello world"</code></td>
    </tr>
    <tr>
      <th colspan="7" align="center">Structs & Enums</th>
    </tr>
    <tr>
      <th>Structs</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>struct MyType {}</code></td>
    </tr>
    <tr>
      <th>Struct Fields</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>struct MyType { filed: i32 }</code></td>
    </tr>
    <tr>
      <th>Linked Functions</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>impl myType {...}</code></td>
    </tr>
    <tr>
      <th>Struct Field Defaults</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>struct MyType { filed: i32 = 1 }</code></td>
    </tr>
    <tr>
      <th>Zero-Size Structs</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>struct Void</code></td>
    </tr>
    <tr>
      <th>Enums</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>enum MyEnum { A, B, C}</code></td>
    </tr>
    <tr>
      <th>Enum Defaults</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>???</td>
    </tr>
    <tr>
      <th>Zero-Size Enums</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>enum Never</code></td>
    </tr>
    <tr>
      <th>Structs<->Enums Composition</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>???</td>
    </tr>
    <tr>
      <th colspan="7" align="center">Polymorphism</th>
    </tr>
    <tr>
      <th>Interfaces|Traits</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>???</td>
    </tr>
    <tr>
      <th>Implementations</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>impl Add for MyType {...}</code></td>
    </tr>
    <tr>
      <th>Static Dispatch</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>???</td>
    </tr>
    <tr>
      <th>Dynamic Dispatch</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>???</td>
    </tr>
    <tr>
      <th><code>where</code> Boundels</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>fn func(arg: T) where T: Default & Add {...}</code></td>
    </tr>
    <tr>
      <th colspan="7" align="center">Metaprogramming & Compile-time</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Algebraic Effects</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Regions</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Compiler Extensions</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Standard Library</th>
    </tr>
  </tbody>
</table>

Higher position - higher priority.

For more information visit [the specification repo](https://github.com/plasm-lang/spec).

## The Compilation Flow

```
                                 HIR                               Codegen (LLVM)
                           ________________             _____________________________________
                          |                |           |                                     |
 code -> tokens -> AST -> | OptHIR -> THIR | -> MIR -> | LLVM-IR -> ASM -----> artifacts ----|--> linker -> Exe
   |        |       |     |_____________|__|     |     |             |     (.o/.a/.bc/.wasm) |
   |        |----> CST                  |        |     |_____________|_______________________|
   |                |                   |        |                   |
   |                |                   |        |                   |
Visible         Visible              Visible  Visible             Visible
in IDE          in IDE               in IDE   in IDE              in IDE


AST         Abstract Syntax Tree
CST         Concrete Syntax Tree
IR          Intermediate Representation
HIR         High-level IR
OptHIR      Optionally typed HIR
THIR        Typed HIR
MIR         Middle-level IR
Codegen     Code Generation
LLVM        A external compiler backend
ASM         Assembler
Exe         Executable file
CLI         Command Line Interface
```

## Sub-Modules Description

- `ast/` - Abstract Syntax Tree + Parser
- `cli/` - Command Line Interface wrapper
- `codegen/` - LLVM-IR + translator from MIR to LLVM-IR + artifacts generation utilities
- `diagnostic/` - Utilities to track errors during compilation process like `Span`, code snippets, and errors container
- `examples/` - Plasm language examples
- `hir/` - High-level IR, OptHIR, THIR + translator from AST + types annotator
- `mir/` - Middle-level IR + translator from HIR
- `orchestrator/` - Compilation flow management utilities (Parallel compilation, linkers controller, configuration reader)
- `tokenizer/` - Tokens description + lexer + from bytes to tokens stream

---

Under [Apache License 2.0](https://github.com/plasm-lang/plasm/blob/main/LICENSE) — forever free to use for any purpose.
