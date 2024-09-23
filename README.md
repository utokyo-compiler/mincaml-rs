# mincaml-rs

a mincaml port written in Rust

## module correspondence

| mincaml        | mincaml-rs                             |       description       |         概要         |
| -------------- | -------------------------------------- | :---------------------: | :------------------: |
| `syntax.ml`    | `syntax/src/lib.rs`                    |    syntax definition    |       構文定義       |
| `lexer.mll`    | `parser/src/lexer`                     |  lexer implementation   |      字句解析器      |
| `parser.mly`   | `parser/src/parser`                    |  parser implementation  |      構文解析器      |
| `type.ml`      | `ty/src/lib.rs`                        | type definition for ast | 構文木のための型定義 |
| `typing.ml`    | `typing/src/lib.rs`                    |     type inference      |        型推論        |
| -              | `ir_typed_ast/`                        |    typed syntax tree    |   型推論後の構文木   |
| `kNormal.ml`   | `ir_knorm/src/{syntax, lowering}.rs`   |     K-normalisation     |       K 正規化       |
| `alpha.ml`     | `typing/src/name_res.rs`               |     name resolution     |       名前解決       |
| `beta.ml`      | `ir_knorm_passes/src/beta_convert.rs`  |                         |                      |
| `assoc.ml`     | `ir_knorm_passes/src/flatten_let.rs`   |     A-normalisation     |       A 正規化       |
| `inline.ml`    | `ir_knorm_passes/src/inline.rs`        |        inlining         |     インライン化     |
| `constFold.ml` | `ir_knorm_passes/src/const_fold.rs`    |    constant folding     |     定数畳み込み     |
| `elim.ml`      | `ir_knorm_passes/src/elim.rs`          |                         |   不要な束縛の除去   |
| `closure.ml`   | `ir_closure/src/{syntax, lowering}.rs` |   closure conversion    |    クロージャ変換    |
| `main.ml`      | `main/src/main.rs`                     |     main entrypoint     |      main 関数       |
