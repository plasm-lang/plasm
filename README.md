# The Plasm Compiler

Experimental, compiled, strongly-typed, functional, system programming language that inherits memory regions and algebraic effects ideas, focused on dev experience, extensibility and transparency.

Mainly inspired by [Rust](https://github.com/rust-lang/rust)'s type system, [Verona](https://github.com/microsoft/verona)'s and [Pony](https://github.com/ponylang/ponyc)'s memory management systems, [Python](https://github.com/python/cpython)'s simplicity, [Koka](https://github.com/koka-lang/koka)'s effect system, and [Zig](https://github.com/ziglang/zig)'s comptime and linking.

### Core Innovative Features

> [!NOTE]
> To keep the barrier to entry low, this document includes academic citations and terminology references. These are intentionally provided to help developers easily familiarize themselves with any unfamiliar computer science paradigms and understand the theoretical foundations behind Plasm's design.

* **Compiler Extensions System** - The compiler is not a monolith, but an open pipeline under orchestrator control. Written entirely in Plasm, extensions can seamlessly replace core frontend stages (like lexers and parsers), introduce domain-specific memory models, or execute compile-time side effects (e.g., CI/CD integrations). Developers can install these external optimization passes, linters, or custom syntactic rules as simply as adding a standard library dependency.
* **Linear Algebraic Effects**<a href="#ref-1"><sup>[1]</sup></a> - A way to abstract asynchronous programming, system calls, RPCs, generators and other side effects via formal effect handlers<a href="#ref-2"><sup>[2]</sup></a> with zero-cost abstractions to solve the Colored Functions Problem<a href="#ref-3"><sup>[3]</sup></a> and the Untracked Effects Problem.
* **Pluggable Memory Management** (backed by Region-based Isolation<a href="#ref-4"><sup>[4]</sup></a> and Reference Capabilities<a href="#ref-5"><sup>[5]</sup></a><a href="#ref-6"><sup>[6]</sup></a><a href="#ref-7"><sup>[7]</sup></a>) - This allows developers to abstract the memory management paradigm or mix multiple strategies simultaneously without compromising safety. Whether enforcing a strict borrow checker with an ownership model, utilizing automatic ARC/RC for shared state, or deploying a localized garbage collector.
* **Advanced IDE Integration** - Integration beyond the standard LSP<a href="#ref-8"><sup>[8]</sup></a>. Developers can instantly inspect all intermediate compilation stages and representations (including desugared syntax, HIR, MIR, LLVM IR, and the emitted assembly) directly within their IDE to understand exactly how their code is being transformed. See the [VS Code extension repository](https://github.com/plasm-lang/vs-code-extension) for more.
* **Compile-Time Meta Programming** - Leverage native syntax to generate code without special DSLs<a href="#ref-9"><sup>[9]</sup></a> (like macros) as well as to access typed metainformation about arbitrary AST nodes during compile-time.

### Design Philosophy

* **Extensibility** - The compiler and language tooling must adapt to the developer's domain, rather than forcing the developer to work around a monolithic black box. Every stage of the compilation pipeline is designed to be an accessible, modular, and replaceable component, empowering engineers to tailor the language semantics to their specific needs.
* **Transparency** - The compilation pipeline must be fully observable and predictable. The language design strictly prohibits "hidden magic": every transformation, such as syntactic desugaring or optimization passes, must be auditable. The compiler guarantees explicit, traceable causality for all actions and errors, eliminating opaque failures (like unexplainable segmentation faults<a href="#ref-10"><sup>[10]</sup></a>) in favor of graceful, highly contextual diagnostics.
* **Readability** - The syntax prioritizes clear intent over extreme brevity. Plasm rejects convoluted, nested declarations (such as the C spiral rule) in favor of linear, straightforward constructs. The goal is simple: an engineer unfamiliar with Plasm should be able to read and understand the logic of a codebase without reading a manual first. Syntactic abbreviations are allowed, but they are strictly limited so they never compromise clarity for an outside observer.
* **One-Way Designability** - There should be exactly one canonical way to express a specific logic or declare a construct. Plasm actively avoids the syntactic fragmentation found in languages like C++, where a single operation can be written in multiple equivalent forms. If multiple approaches must exist for different contexts, the distinction is unambiguous and strictly enforced by the default linter, ensuring a uniform, highly predictable codebase across the entire ecosystem.
* **Runtime Performance & Safety > Compilation Time** - The quality of the final executable is paramount. Plasm explicitly prioritizes aggressive optimization passes and exhaustive compile-time verification over rapid build times. While fast developer iteration is fully supported through stripped-down debug profiles, production builds will never sacrifice execution speed or safety just to compile faster. This establishes a definitive architectural priority: if a heavy compiler pass prevents a runtime bug or extracts maximum performance, it is always worth the wait.

> #### References and Terms
> 1.  <a id="ref-1"></a> D. Leijen. "Koka: Programming with Row Polymorphic Effect Types" in Electronic Proceedings in Theoretical Computer Science, vol. 153, pp. 100–126, 2014. DOI: [10.48550/arXiv.1406.2061](https://doi.org/10.48550/arXiv.1406.2061)
> 2.  <a id="ref-2"></a> M. Plotkin, "Handlers of Algebraic Effects" in Programming Languages and Systems, 2009, pp. 80–94. DOI: [10.1007/978-3-642-00590-9_7](https://doi.org/10.1007/978-3-642-00590-9_7).
> 3.  <a id="ref-3"></a> Nystrom, R. "What color is your function?" in Stuff with Stuff, 2015, https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function
> 4.  <a id="ref-4"></a> Grossman, D., et al. "Region-based memory management in cyclone" in SIGPLAN Not., vol. 37, no. 5, pp. 282–293, 2002. DOI: [10.1145/543552.512563](https://doi.org/10.1145/543552.512563).
> 5.  <a id="ref-5"></a> P. Wadler, "Linear types can change the world!" in Programming concepts and methods, 1990, pp. 5.
> 6.  <a id="ref-6"></a> Arvidsson, E., et al. "Reference Capabilities for Flexible Memory Management" in Proc. ACM Program. Lang., vol. 7, no. OOPSLA2, 2023. DOI: [10.1145/3622846](https://doi.org/10.1145/3622846).
> 7.  <a id="ref-7"></a> Clebsch, S., et al, "Deny capabilities for safe, fast actors" in Proceedings of the 5th International Workshop on Programming Based on Actors, Agents, and Decentralized Control, 2015, pp. 1–12. DOI: [10.1145/2824815.2824816](https://doi.org/10.1145/2824815.2824816).
> 8.  <a id="ref-8"></a> "Language Server Protocol" in Wikipedia, 2026, https://en.wikipedia.org/wiki/Language_Server_Protocol
> 9.  <a id="ref-9"></a> "Domain-specific language" in Wikipedia, 2026, https://en.wikipedia.org/wiki/Domain-specific_language
> 10. <a id="ref-10"></a> "Segmentation fault" in Wikipedia, 2026, https://en.wikipedia.org/wiki/Segmentation_fault


## Quick Start

Install from source using Cargo:

```shell
cargo install --path cli
```
> [!NOTE]
> Requires the `zstd` library to build the compiler. On Ubuntu: `sudo apt install libzstd-dev`.

For cli help reference:

```shell
plasm --help
```

For example, following command will parse `test.sm` file, analyse, stop on assembly stage and emit the result in a text format:

```shell
plasm emit test.sm --stage asm
```

## Status

### Early Stage Roadmap (MVP)

This is the roadmap for implementing a minimal working framework that will serve as the base for the proposed innovative features.

- [x] Lexer & tokens stream
- [x] AST definition
- [x] AST parser
- [x] HIR/OptHIR/THIR definitions
- [x] AST to OptHIR translator
- [x] OptHIR to THIR translator (type inference)
- [x] MIR definition
- [X] THIR to MIR lowering
- [x] MIR to LLVM-IR lowering
- [ ] LLVM-IR to object file compilation
- [ ] Object files linking

### Features Status Matrix

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
      <td>✅</td>
      <td><code>let x = 1</code></td>
    </tr>
    <tr>
      <th>Function Definition</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td><code>fn func() {}</code></td>
    </tr>
    <tr>
      <th>Input Args</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td><code>fn func(arg: i32) {}</code></td>
    </tr>
    <tr>
      <th>Output Args</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td><code>fn func() -> i32 {}</code></td>
    </tr>
    <tr>
      <th>Function Calls</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td><code>func()</code></td>
    </tr>
    <tr>
      <th>Argument Passing</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td><code>func(var1, var2)</code></td>
    </tr>
    <tr>
      <th>Foreign Functions (C FFI)</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td><code>fn f(x: i32) -> i32</code></td>
    </tr>
    <tr>
      <th>Binary & Unary Operations</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>let y = x + 2</code></td>
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
      <th>Module System</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>import some_module</code></td>
    </tr>
    <tr>
      <th>If</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>if cond {...}</code></td>
    </tr>
    <tr>
      <th>While</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>while cond {...}</code></td>
    </tr>
    <tr>
      <th colspan="7" align="center">Typing System</th>
    </tr>
    <tr>
      <th>Primitive Types</th>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td>✅</td>
      <td><code>bool, u8, i32, i1024, f64<code></td>
    </tr>
    <tr>
      <th>Tuples</th>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>(i32, i32, i32)</code></td>
    </tr>
    <tr>
      <th>Structs</th>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>type MyType = struct {}</code></td>
    </tr>
    <tr>
      <th>Struct Fields</th>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>type MyType = struct { filed: i32 }</code></td>
    </tr>
    <tr>
      <th>Reference Type</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>&x</code></td>
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
      <th>String Literals</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>let a = "Hello world"</code></td>
    </tr>
    <tr>
      <th>Struct Field Defaults</th>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>type MyType = struct { filed: i32 = 1 }</code></td>
    </tr>
    <tr>
      <th>Zero-Size Structs</th>
      <td>✅</td>
      <td>✅</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>type Void = struct {}</code></td>
    </tr>
    <tr>
      <th>Enums</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>type MyEnum = enum { A, B, C }</code></td>
    </tr>
    <tr>
      <th>Enum Defaults</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>WIP</td>
    </tr>
    <tr>
      <th>Zero-Size Enums</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>type Never = enum {}</code></td>
    </tr>
    <tr>
      <th>Structs & Enums Composition</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>See Examples Folder</td>
    </tr>
    <tr>
      <th>Unions</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>A | B | C</code></td>
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
      <td>WIP</td>
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
      <td>WIP</td>
    </tr>
    <tr>
      <th>Dynamic Dispatch</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>WIP</td>
    </tr>
    <tr>
      <th><code>where</code> Bounds</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>fn func(arg: T) where T: Default & Add {...}</code></td>
    </tr>
    <tr>
      <th>For</th>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td>⬜</td>
      <td><code>for val in iter {}</code></td>
    </tr>
    <tr>
      <th colspan="7" align="center">Metaprogramming & Compile-time</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Compiler Extensions</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Algebraic Effects</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Memory Regions</th>
    </tr>
    <tr>
      <th colspan="7" align="center">Standard Library</th>
    </tr>
  </tbody>
</table>

For more information visit [the specification repo](https://github.com/plasm-lang/spec).

## Contributing

Plasm is currently in an active phase of core development, and open-source contributions are highly encouraged. Whether you are interested in implementing a specific feature from the roadmap, refining intermediate representations, or optimizing the compilation pipeline, your participation is welcome.

There is no strict contribution flow at this stage. If you would like to take on a task, propose an architectural improvement, or simply participate in the development process, please join our community server and let us know what you want to work on.

[![Discord](https://img.shields.io/badge/Discord-Server-5865F2?style=for-the-badge&logo=discord&logoColor=white)](https://discord.gg/FwBDKTAjnw)

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
LLVM        An external compiler backend
ASM         Assembly code
Exe         Executable file
CLI         Command Line Interface
```

## Sub-Modules Description

- `ast/` - Abstract Syntax Tree + Parser
- `cli/` - Command Line Interface wrapper
- `codegen/` - LLVM IR + translator from MIR to LLVM IR + artifacts generation utilities
- `diagnostic/` - Utilities to track errors during compilation process like `Span`, code snippets, and errors container
- `examples/` - Plasm language examples
- `hir/` - High-level IR, OptHIR, THIR + translator from AST + types annotator
- `mir/` - Middle-level IR + translator from HIR
- `orchestrator/` - Compilation flow management utilities (Parallel compilation, linkers controller, configuration reader)
- `tokenizer/` - Tokens description + lexer + from bytes to tokens stream
- `utils/` - Common utilities shared across most modules (e.g., shared data types)

---

Under [Apache License 2.0](https://github.com/plasm-lang/plasm/blob/main/LICENSE) — forever free to use for any purpose.
