# mincaml-rs

a mincaml port written in Rust

## file correspondence

| mincaml      | mincaml-rs                    |
| ------------ | ----------------------------- |
| syntax.ml    | syntax/src/lib.rs             |
| lexer.mll    | parser/src/lexer.rs           |
| parser.mly   | parser/src/parser.rs          |
| type.ml      | ty/src/lib.rs                 |
| typing.ml    | typing/src/lib.rs             |
| kNormal.ml   | ir_knorm/src/lib.rs           |
| alpha.ml     | ir_knorm/src/alpha_convert.rs |
| beta.ml      | ir_knorm/src/beta_convert.rs  |
| assoc.ml     | ir_knorm/src/flatten_let.rs   |
| inline.ml    | ir_knorm/src/inline.rs        |
| constFold.ml | ir_knorm/src/const_fold.rs    |
| elim.ml      | ir_knorm/src/elim.rs          |
| main.ml      | main/src/main.rs              |
