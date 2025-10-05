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
                                 HIR                             Codegen (LLVM)
                           ________________             _______________________________
                          |                |           |                               |
 code -> tokens -> AST -> | OptHIR -> THIR | -> MIR -> | LLVM-IR -> ASM -> object file | -> linker -> Exe
   |        |       |     |_____________|__|     |     |_____________|_________________|
   |        |----> CST                  |        |                   |
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
