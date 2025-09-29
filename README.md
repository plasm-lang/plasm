# Sky programming language

For cli help reference:

```shell
cargo run --help
```

For example, following command will parse `test.sky` file, stop on AST stage and emit the result in text format:

```shell
cargo run emit test.sky --stage ast --format text
```
