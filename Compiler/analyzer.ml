(* Semantic Analyzer 
	- Consumes abstract syntax tree from parser
	- Produces semantically Analyzed Syntax Tree for compiler
*)

open Ast
open Sast

(* Exceptions *)
exception Except of string

(* Variable Declarations *)
type var_decl = { type : Ast.vtype;
                  name : string; }

(* Symbol Table *)
(* we may not need a parent symbol table if we don't have nested scope *)
type symbol_table = { parent : symbol_table option;
                      mutable variables : var_decl list; 
                      int }

(* Environment *)
type environment = { scope : symbol_table;
                     formal_params : var_decl list option
                     return_type : Ast.vtype }

(* Utility Functions *)

(* Lookup variable in symbol table and return its type *)
(* we may not need to make this recursive if we don't have nested scope *)
let rec find_var (scope : symbol_table) name =
  try
    let (_, t) = List.find (fun vdecl -> name = vdecl.name) scope.variables in t
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_var parent name
      | _ -> raise Not_found

(* Checks *)

(* Check IDs *)
let check_id name env =
  let vdecl =
    try
      find_var env.scope name
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name)) in
         let t = vdecl.type in
           Sast.Expr(Sast.Id(name), t)

(* Checks unops *)
let check_unop op e env =
    let e = check_expr env e in
    match e with
      Sast.Expr(_, t) ->
        if op = Ast.Uminus then
        if t = Ast.Num then
          Sast.Expr(Sast.Unop(op, e), Ast.Num)
        else
          if t = Ast.Func then
            Sast.Expr(Sast.Unop(op, e), Ast.Func)
          else
            if t = Ast.Matrix then
              Sast.Expr(Sast.Unop(op, e), Ast.Matrix)
            else raise (Error("Illegal Unary Operator!"))
        else
        if op = Ast.Not then
              if t = Ast.Num then
            Sast.Expr(Sast.Unop(op, e), Ast.Num)
          else
            if t = Ast.Func then
              Sast.Expr(Sast.Unop(op, e), Ast.Func)
            else raise (Error("Illegal Unary Operator!"))
        else raise (Error("Illegal Unary Operator!"))

(* Checks binops *)

(* Checks expressions *)

let check_expr env = function
  Ast.Lit_int(s) -> Sast.Expr(Sast.Litnum(s), Ast.Num)
  | Ast.Litstring(s) -> Sast.Expr(Sast.Litstring(s), Ast.String)
  | Ast.Litfunc(l, f_expr) -> Sast.Expr(Sast.Litfunc(l, check_fexpr l (convert_fexpr f_expr) env), Ast.Func)
  | Ast.Litlist(l) -> check_list l env
  | Ast.Litmatrix(l) -> check_matrix l env
  | Ast.Access(s, el) -> check_access s el env
  | Ast.Id(name) -> check_id name env
  | Ast.Binop(e1, op, e2) -> check_binop e1 op e2 env
  | Ast.Unop(op, e) -> check_unop op e env
  | Ast.Call(name, args) -> check_scall name args env
  | Ast.FCall(fcall) -> check_fcall fcall env
  | _ -> raise (Error("Invalid expression"))

  let check_expr env = function
  Ast.Lit_int(x) -> Sast.Expr(Sast.Lit_int(x), Ast.Int)
  | Ast.Lit_float(x) -> Sast.Expr(Sast.Lit_float(x), Ast.Float)
  | Ast.Com(x) -> check_com x env
  | Ast.Qub_bra(x) -> check_qub_bra x env
  | Ast.Qub_ket(x) -> check_qub_ket x env
  | Ast.Mat(x) -> check_mat x env
  | Ast


  Lit_int of int
  | Lit_float of float
  | Com of float * float
  | Qub_bra of expr
  | Qub_ket of expr
  | Mat of expr list list
  | Id of string
  | Unop of un_op * expr
  | Binop of expr * bi_op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr 


