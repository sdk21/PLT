(* Semantic Analyzer 
	- Consumes abstract syntax tree from parser
	- Produces semantically Analyzed Syntax Tree for compiler
*)

open Ast
open Sast

(* Exceptions *)
exception Except of string

(* Variable Declarations *)
type var_decl = { typ : Ast.data_type;
                  name : string; }

(* Symbol Table *)
(* we may not need a parent symbol table if we don't have nested scope *)
type symbol_table = { parent : symbol_table option;
                      mutable variables : var_decl list; }

(* Environment *)
type environment = { scope : symbol_table;
                     formal_params : var_decl list option;
                     return_type : Ast.data_type; }

(*******************
 *Utility Functions*
*******************)

(* Lookup variable in symbol table *)
(* we may not need to make this recursive if we don't have nested scope *)
let rec find_var (scope : symbol_table) name =
  try
    let vdecl_found = List.find (fun vdecl -> name = vdecl.name) scope.variables in vdecl_found
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_var parent name
      | _ -> raise Not_found

(************
 *Exceptions*
************)

(* Throws errors associated with unary operators *)
let unop_error t = match t with
  Ast.Not -> raise (Except("Invalid use of 'Not(expr)'"))
  | Ast.Re -> raise (Except("Invalid use of 'Re(expr)'"))
  | Ast.Im -> raise (Except("Invalid use of 'Im(expr)'"))
  | Ast.Norm -> raise (Except("Invalid use of 'Norm(expr)'"))
  | Ast.Trans -> raise (Except("Invalid use of 'Trans(expr)'"))
  | Ast.Det -> raise (Except("Invalid use of 'Det(expr)'"))
  | Ast.Adj -> raise (Except("Invalid use of 'Adj(expr)'"))
  | Ast.Conj -> raise (Except("Invalid use of 'Conjexpr)'"))
  | Ast.Unit -> raise (Except("Invalid use of 'Unit(expr)'"))
  | Ast.Sin -> raise (Except("Invalid use of 'Sin(expr)'"))
  | Ast.Cos -> raise (Except("Invalid use of 'Cos(expr)'"))
  | Ast.Tan -> raise (Except("Invalid use of 'Tan(expr)'"))

(* Throws erros associated with binary operators *)
let binop_error t = match t with
  Ast.Add -> raise (Except("Invalid use of 'expr + expr'"))
  | Ast.Sub -> raise (Except("Invalid use of 'expr - expr'"))
  | Ast.Mult -> raise (Except("Invalid use of 'expr * expr'"))
  | Ast.Div -> raise (Except("Invalid use of 'expr / expr'"))
  | Ast.Mod -> raise (Except("Invalid use of 'expr % expr'"))
  | Ast.Expn -> raise (Except("Invalid use of 'expr ^ expr'"))
  | Ast.Or -> raise (Except("Invalid use of 'expr or expr'"))
  | Ast.And -> raise (Except("Invalid use of 'expr and expr'"))
  | Ast.Xor -> raise (Except("Invalid use of 'expr xor expr'"))
  | Ast.Tens -> raise (Except("Invalid use of 'expr @ expr'"))
  | Ast.Eq -> raise (Except("Invalid use of 'expr eq expr'"))
  | Ast.Neq -> raise (Except("Invalid use of 'expr neq expr'"))
  | Ast.Lt -> raise (Except("Invalid use of 'expr lt expr'"))
  | Ast.Gt -> raise (Except("Invalid use of 'expr gt expr'"))
  | Ast.Leq -> raise (Except("Invalid use of 'expr leq expr'"))
  | Ast.Geq -> raise (Except("Invalid use of 'expr geq expr'"))

(********
 *Checks*
********)

(* Check IDs *)
let check_id name env =
  let vdecl =
    try
      find_var env.scope name
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name))
  in
    let typ = vdecl.typ in
      Sast.Expr(Sast.Id(name), typ)


(*
(* Check Assignments *)
let check_assign name l e env =

(* Checks expressions *)
let check_expr env = match function
  Ast.Lit_int(e) -> Sast.Expr(Sast.Lit_int(e), Ast.Int)
  | Ast.Lit_float(e) -> Sast.Expr(Sast.Lit_float(e), Ast.Float)
  | Ast.Comp(e1, e2) -> 
  | Ast.Qub_bra(e) ->
  | Ast.Qub_ket(e) ->
  | Ast.Mat(e) ->
  | Ast.Id(e) ->
  | Ast.Unop(op, e) ->
  | Ast.Binop(e1, op, e2) ->
  | Ast.Assign(e1, e2) -> 
  | Ast.Call(e1, e2) ->
  | _ -> raise (Except("Invalid expression"))
*)

(* Checks unary operators *)
let check_unop op e env =
  let e = check_expr e env in 
    match e with
      Sast.Expr(_, t) ->
        (match op with
          Ast.Not ->
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Ast.Int)
              | _ ->  unop_error op)
          | Ast.Re ->
            (match t with
              Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Ast.Comp)
              | _ ->  unop_error op)
          | Ast.Im -> 
            (match t with
                Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Ast.Comp)
              | _ ->  unop_error op)
          | Ast.Norm ->
            (match t with
              Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Ast.Comp)
              | Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Ast.Mat)
              | Ast.Qub -> Sast.Expr(Sast.Unop(op, e), Ast.Qub)
              | _ ->  unop_error op)
          | Ast.Trans ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Ast.Mat)
              | _ ->  unop_error op)
          | Ast.Det ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Ast.Mat)
              | _ ->  unop_error op)
          | Ast.Adj ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Ast.Mat)
              | _ ->  unop_error op)
          | Ast.Conj ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Ast.Mat)
              | _ ->  unop_error op)
          | Ast.Unit -> 
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Ast.Mat)
              | _ ->  unop_error op)
          | Ast.Sin -> 
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Ast.Int)
              | Ast.Float -> Sast.Expr(Sast.Unop(op, e), Ast.Float)
              | Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Ast.Comp)
              | _ ->  unop_error op)
          | Ast.Cos -> 
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Ast.Int)
              | Ast.Float -> Sast.Expr(Sast.Unop(op, e), Ast.Float)
              | Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Ast.Comp)
              | _ ->  unop_error op)
          | Ast.Tan -> 
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Ast.Int)
              | Ast.Float -> Sast.Expr(Sast.Unop(op, e), Ast.Float)
              | Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Ast.Comp)
              | _ ->  unop_error op))

(* Checks binary operators *)
let check_binop e1 op e2 env =
    let e1 = check_expr env e1 and e2 = check_expr env e2 in
      match e1 with
        Sast.Expr(_, t1) ->
          (match e2 with
            Sast.Expr(_, t2) ->
              (match op with
                Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Expn -> 
                  (match t1 with
                    Ast.Int -> 
                      (match t2 with
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Int)
                        | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Float)
                        | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Comp)
                        | _ -> binop_error op)
                    | Ast.Float -> 
                      (match t2 with
                        Ast.Int | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Float)
                        | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Comp)
                        | _ -> binop_error op)
                    | Ast.Comp -> 
                      (match t2 with
                        Ast.Int | Ast.Float | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Comp)
                        | _ -> binop_error op)
                    | Ast.Qub ->
                      (match t2 with 
                        Ast.Qub -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Qub)
                       |  _ -> binop_error op)
                    | Ast.Mat ->
                      (match t2 with
                        Ast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Mat)
                        | _ -> binop_error op))
                | Ast.Tens -> 
                  (match t1 with
                    Ast.Mat ->
                      (match t2 with
                        Ast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Mat)
                        |  _ -> binop_error op)
                      | _ -> binop_error op)
                | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq ->
                  (match t1 with
                    Ast.Int ->
                     (match t2 with
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Int)
                       | _ -> binop_error op)
                  | Ast.Float -> 
                      (match t2 with
                        Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Float)
                        | _ -> binop_error op)
                  | Ast.Comp -> 
                      (match t2 with
                        Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Float)
                        | _ -> binop_error op)
                  | Ast.Qub -> 
                      (match t2 with
                        Ast.Qub -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Qub)
                        |  _ -> binop_error op)
                  | Ast.Mat ->
                      (match t2 with
                        Ast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Mat)
                        |  _ -> binop_error op))
                | Ast.Or | Ast.And | Ast.Xor -> 
                  (match t1 with
                    Ast.Int ->
                      (match t2 with
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Int)
                        | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Float)
                        |  _ -> binop_error op)
                    | _ -> binop_error op)))
