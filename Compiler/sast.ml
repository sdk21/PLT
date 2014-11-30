(* Semantically Analyzed Syntax Tree
    - Produced by semantic analyzer
    - Consumed by compiler
*)

open Ast

type sdata_type =
    Int
  | Float
  | Comp
  | Mat
  | Mat_int
  | Mat_float
  | Mat_comp
  | Qub
  | Qub_bra
  | Qub_ket

(* Expressions *)
type expr_wrapper = 
    Expr of sexpr * sdata_type

and  sexpr =
  Lit_int of int
  | Lit_float of float
  | Lit_comp of float * float
  | Qub of expr_wrapper
  | Mat of expr_wrapper list list
  | Id of string
  | Unop of Ast.un_op * expr_wrapper
  | Binop of expr_wrapper * Ast.bi_op * expr_wrapper
  | Assign of string * expr_wrapper
  | Call of string * expr_wrapper list
  | Noexpr

(* Statements *)
and sstmt =
    Block of sstmt list
  | If of expr_wrapper * sstmt
  | For of expr_wrapper * expr_wrapper * expr_wrapper * expr_wrapper * sstmt
  | While of expr_wrapper * sstmt
 
(* Variables Declaration *)
and svar_decl = 
  { 
    styp : sdata_type;
    sname : string;
  }

(* Function Declaration *)
and sfunc_decl = 
  {
    sret_type : sdata_type;
    sret_name : string;
    sfunc_name : string;
    sformal_params : svar_decl list;
    slocals : svar_decl list;
    sbody : sstmt list;
  }

(* Program *)
type sprogram =
  sfunc_decl list
