# Sky programming language

For cli help reference:

```shell
cargo run --help
```

For example, following command will parse `test.sky` file, stop on AST stage and emit the result in text format:

```shell
cargo run emit test.sky --stage ast --format text
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
CLI         Computer Line Interface
```

## Sub-modules desription

- `ast/` - Abstract Syntax Tree + Parser
- `cli/` - Computer Line Interface wrapper
- `codegen/` - LLVM-IR + translator from MIR to LLVM-IR + artifacts generation utilities
- `diagnostic/` - Utilities to track errors during compilation process like `Span`, code snippets, and errors container
- `examples/` - Plasm language examples
- `hir/` - High-level IR, OptHIR, THIR + translator from AST + types annotator
- `mir/` - Middle-level IR + translator from HIR
- `orchestrator/` - Compilation flow management utilities (Parallel compilation, linker controller, configuration reader)
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
