(* Semantically Analyzed Syntax Tree
    - Produced by semantic analyzer
    - Consumed by compiler
*)


open Ast

type expr_wrapper =
    Expr of sexpr * Ast.v_type

