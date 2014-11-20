(* Semantically Analyzed Syntax Tree
    - Produced by semantic analyzer
    - Consumed by compiler
*)

open Ast

(* Expressions *)
type expr_wrapper = 
    Expr of sexpr * Ast.data_type

and  sexpr =
  Lit_int of int
  | Lit_float of float
  | Comp of float * float
  | Qub_bra of expr_wrapper
  | Qub_ket of expr_wrapper
  | Mat of expr_wrapper list list
  | Id of string
  | Unop of Ast.un_op * expr_wrapper
  | Binop of expr_wrapper * Ast.bi_op * expr_wrapper
  | Assign of string * expr_wrapper
  | Call of string * expr_wrapper list
  | Noexpr

(* Statements *)
and sstmt =
  Exprst of expr_wrapper
  | Block of sstmt list
  | If of expr_wrapper * sstmt
  | For of expr_wrapper * expr_wrapper * expr_wrapper * expr_wrapper * sstmt
  | While of expr_wrapper * sstmt
 
(* Variables Declaration *)
and svar_decl = 
  { 
    svar_type : Ast.data_type;
    svar_name : string;
  }

(* Function Declaration *)
and sfunc_decl = 
  {
    ret_type : Ast.data_type;
    ret_name : string;     (*==== Do we need this??=====*)
    func_name : string;
    formal_params : svar_decl list;
    locals : svar_decl list;
    body : sstmt list;
  }

(* Program *)
type sprogram =
  svar_decl list * sfunc_decl list