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

(* Unary operator errors *)
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

(* Binary operator errors *)
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

(* Qubit erros *)
let qub_error t = match t with
  0 -> raise (Except("Invalid use of <expr|"))
  | 1 -> raise (Except("Invalid use of |expr>"))

(* Matrix errors *)
let matrix_error t1 t2 = match t2 with
  0 -> raise (Except("Invalid type in matrix"))
  | _ -> raise (Except("Type mismatch in matrix"))

(********
 *Checks*
********)

(* Checks qubit expression (all 0s and 1s?) *)
let rec check_qub_expr e =
  let r = e mod 10 in
   if (r = 0 || r = 1) then
     let e = e / 10 in
       if (e != 0) then
         check e
       else 1
    else 0

(* Checks qubits *)
let check_qub e bk env =
  let Sast.Expr(_, t) = check_expr e env in
    if t = Ast.Int then
      if (check_qub_expr e env) then
        match bk with
          0 -> Sast.Expr(Sast.Qub(e), Sast.Qub_bra)
          | 1 -> Sast.Expr(Sast.Qub(e), Sast.Qub_ket)
    else
      qub_err t

(* Check matricies *)
let check_mat l env = 
  List.map check_mat_exprs_helper l env

(* Check matrix expressions helper *)
let check_mat_exprs_helper l env =
  let e = (List.hd l) in
    let Sast.Expr(_, t) = check_expr e env in 
      match t with
        Ast.Int -> check_mat_exprs l Ast.Int env
        | Ast.Float -> check_mat_exprs l Ast.Float env
        | Ast.Comp -> check_mat_exprs l Ast.Comp env
        | _ -> matrix_error t 0

(* Check matrix expressions *)
let rec check_mat_exprs l t1 env =
  let e = (List.hd l) in
    let Sast.Expr(_, t2) = check_expr e env in 
      match t2 with
        t1 -> let row_tail = (List.tl l) in
                match row_tail with
                  [] -> 1
                  | _ -> check_mat_exprs row_tail t1 env
        | _ -> matrix_error t1 t2

(* Check IDs *)
let check_id name env =
  let vdecl =
    try
      find_var env.scope name
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name))
  in
    let t = vdecl.typ in
      Sast.Expr(Sast.Id(name), t)

(* Check assignments *)
let check_assign name l e env =

(* Checks expressions *)
let check_expr env = match function
  Ast.Lit_int(i) -> Sast.Expr(Sast.Lit_int(i), Ast.Int)
  | Ast.Lit_float(f) -> Sast.Expr(Sast.Lit_float(f), Ast.Float)
  | Ast.Comp(f1, f2) -> Sast.Expr(Sast.Lit_comp(f1, f2), Ast.Comp)
  | Ast.Qub(e, bk) -> check_qub e bk env
  | Ast.Mat(l) -> check_mat l env
  | Ast.Id(d) -> check_id d env
  | Ast.Unop(op, e) -> check_unop op e env
  | Ast.Binop(e1, op, e2) -> check_binop e1 op e2 env
  | Ast.Assign(s, e) -> check_assign e1 e2 env
  | Ast.Call(s, l) -> check_call s l env
  | _ -> raise (Except("Invalid expression"))

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
              | Sast.Qub_bra -> Sast.Expr(Sast.Unop(op, e), Sast.Qub_bra)
              | Sast.Qub_ket -> Sast.Expr(Sast.Unop(op, e), Sast.Qub_ket)
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
                Ast.Add | Ast.Sub | Ast.Mult | Ast.Div -> 
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
                    | Ast.Mat ->
                      (match t2 with
                        Ast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Mat)
                        | _ -> binop_error op))
                    | Sast.Qub_bra ->
                      (match t2 with 
                        Sast.Qub_bra -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qub_bra)
                       |  _ -> binop_error op)
                    | Sast.Qub_ket ->
                      (match t2 with 
                        Sast.Qub_ket -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qub_ket)
                       |  _ -> binop_error op)
                | Ast.Mod | Ast.Expn ->
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
                    | _ -> binop_error op)
                | Ast.Tens -> 
                  (match t1 with
                    Ast.Mat ->
                      (match t2 with
                        Ast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Mat)
                        |  _ -> binop_error op)  
                    | Sast.Qub_bra ->
                      (match t2 with 
                        Sast.Qub_bra -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qub_bra)
                       |  _ -> binop_error op)
                    | Sast.Qub_ket ->
                      (match t2 with 
                        Sast.Qub_ket -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qub_ket)
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
                  | _ -> binop_error op)
                | Ast.Or | Ast.And | Ast.Xor -> 
                  (match t1 with
                    Ast.Int ->
                      (match t2 with
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Int)
                        | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Ast.Float)
                        |  _ -> binop_error op)
                    | _ -> binop_error op)))
