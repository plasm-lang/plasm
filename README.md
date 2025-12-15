# The Plasm compiler

Experimental, compiled, strongly-typed, functional, system programming language that inherits memory regions ideas, focused on dev experience, extensibility and reliability.

Mainly inspired by [Rust](https://github.com/rust-lang/rust)'s type system, [Verona](https://github.com/microsoft/verona)'s and [Pony](https://github.com/ponylang/ponyc)'s memory managment systems, [Python](https://github.com/python/cpython)'s simplicity, [Koka](https://github.com/koka-lang/koka)'s effect system, and [Zig](https://github.com/ziglang/zig)'s comptime.

## Early stage roadmap (MVP)

This is the roadmap for implementing a minimal working framework that will serve as the base for the proposed innovative features. Basically, this is the way to create a "Rustic C" compiler.

- [x] Lexer & tokens stream
- [x] AST definition
- [x] AST parser
- [x] HIR/OptHIR/THIR definitions
- [x] AST to OptHIR translator
- [x] OptHIR to THIR translator (type inference)
- [ ] MIR definition
- [ ] THIR to MIR lowering
- [ ] MIR to LLVM-IR lowering
- [ ] LLVM-IR to object file compilation
- [ ] object files linking

## Quick Start

For cli help reference:

```shell
cargo run --help
```

For example, following command will parse `test.sm` file, analyse, stop on HIR stage and emit the result in a text format:

```shell
cargo run emit test.sm --stage hir --format text
```

## The compilation flow

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

## Sub-modules description

- `ast/` - Abstract Syntax Tree + Parser
- `cli/` - Command Line Interface wrapper
- `codegen/` - LLVM-IR + translator from MIR to LLVM-IR + artifacts generation utilities
- `diagnostic/` - Utilities to track errors during compilation process like `Span`, code snippets, and errors container
- `examples/` - Plasm language examples
- `hir/` - High-level IR, OptHIR, THIR + translator from AST + types annotator
- `mir/` - Middle-level IR + translator from HIR
- `orchestrator/` - Compilation flow management utilities (Parallel compilation, linkers controller, configuration reader)
- `tokenizer/` - Tokens description + lexer + from bytes to tokens stream

Internal dependencies:

```
orchestrator    diagnostic -> tokenizer -> ast -> hir -> mir -> codegen
      ^            | |            |        ^ |    ^ |     |        |
      |            | |            |        | |    | |     |        |
      |            |_|____________|________|_|____| |     |        |
      |              |            |          |      |     |        |
      |______________|____________|__________|______|_____|________|
```
