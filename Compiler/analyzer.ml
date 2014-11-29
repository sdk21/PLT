(* Semantic Analyzer 
	- Consumes abstract syntax tree from parser
	- Produces semantically Analyzed Syntax Tree for compiler
*)

open Ast
open Sast

(* Exceptions *)
exception Except of string

(* Symbol Table *)
(* we may not need a parent symbol table if we don't have nested scope *)
type symbol_table = { parent : symbol_table option;
                      formal_params : Sast.svar_decl list option;
                      return_type : Sast.sdata_type;
                      mutable variables : Sast.svar_decl list; }

(* Environment *)
type environment = { scope : symbol_table;
                     mutable functions : Sast.sfunc_decl list; }

(* Root symbol table *)
let root_symbol_table =
  {parent = None; formal_params = None; return_type = None; variables = None}

(* Root environment *)
let root_environment = 
  {scope = root_symbol_table; functions = None}

(************
 *Exceptions*
************)

(* Matrix errors *)
let matrix_error t1 t2 = match t2 with
  0 -> raise (Except("Invalid type in matrix"))
  | _ -> raise (Except("Type mismatch in matrix"))

(* Qubit erros *)
let qub_error t = match t with
  0 -> raise (Except("Invalid use of <expr|"))
  | 1 -> raise (Except("Invalid use of |expr>"))
  | _ -> raise (Except("Invalid use qubits"))

(* Unary operator errors *)
let unop_error t = match t with
  Ast.Neg -> raise (Except("Invalid use of '-expr'"))
  | Ast.Not -> raise (Except("Invalid use of 'Not(expr)'"))
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

(* Expressions *)
let expr_error =
  raise (Except("Invalid expression"))

(* Statements *)

(* Program *)
let program_error s =
  raise (Except("Invalid program caused by " ^ s ^ " declaration"))

(*******************
 *Utility Functions*
*******************)

(* Lookup variable in symbol table *)
(* we may not need to make this recursive if we don't have nested scope *)
let rec lookup_var name scope =
  try
    let vdecl_found = List.find (fun vdecl -> name = vdecl.sname) scope.variables in vdecl_found
  with Not_found ->
    match scope.parent with
      Some(parent) -> lookup_var name parent
      | _ -> raise Not_found

(* Lookup function in environment *)
let rec lookup_func name args env =
  try
    let fdecl_found = List.find (fun fdecl -> name = fdecl.sfunc_name) scope.variables in fdecl_found
  with Not_found -> raise Not_found

(********
 *Checks*
********)

  (* Checks expressions *)
let rec check_expr env = function
  Ast.Lit_int(i) -> Sast.Expr(Sast.Lit_int(i), Sast.Int)
  | Ast.Lit_float(f) -> Sast.Expr(Sast.Lit_float(f), Sast.Float)
  | Ast.Lit_comp(f1, f2) -> Sast.Expr(Sast.Lit_comp(f1, f2), Sast.Comp)
  | Ast.Qub(e, bk) -> check_qub e bk env
  (*| Ast.Mat(l) -> check_mat l env*)
  | Ast.Id(s) -> check_id s env
  | Ast.Unop(op, e) -> check_unop op e env
  | Ast.Binop(e1, op, e2) -> check_binop e1 op e2 env
  | Ast.Assign(s, e) -> check_assign s e env
  | Ast.Call(s, l) -> check_call s l env
  | _ -> expr_error

(* Checks qubit expression (all 0s and 1s?) *)
and check_qub_expr e env =
  let r = e mod 10 in
   if (r = 0 || r = 1) then
     let e = e / 10 in
       if (e != 0) then
         check_qub_expr e env
       else 1
    else 0

(* Checks qubits *)
and check_qub e bk env =
  let e1 = check_expr env e in
    match e1 with
      Sast.Expr(e2, t) ->
        (match e2 with
          Sast.Lit_int(i) ->
            if (check_qub_expr i env = 1) then
              (match bk with
                0 -> Sast.Expr(Sast.Qub(e1), Sast.Qub_bra)
                | 1 -> Sast.Expr(Sast.Qub(e1), Sast.Qub_ket)
                | _ -> qub_error bk)
            else
              qub_error bk
          | _ -> qub_error bk)
      | _ -> qub_error bk

(* Check matricies
and check_mat l env = 
  List.map (check_mat_exprs_helper l env);

(* Check matrix expressions helper *)
and check_mat_exprs_helper l env =
  let e = (List.hd l) in
    let Sast.Expr(_, t) = check_expr e env in 
      match t with
        Sast.Int -> check_mat_exprs l Sast.Int env
        | Sast.Float -> check_mat_exprs l Sast.Float env
        | Sast.Comp -> check_mat_exprs l Sast.Comp env
        | _ -> matrix_error t 0

(* Check matrix expressions *)
and check_mat_exprs l t1 env =
  let e = (List.hd l) in
    let Sast.Expr(_, t2) = check_expr e env in 
      match t2 with
        t1 -> let row_tail = (List.tl l) in
                match row_tail with
                  [] -> 1
                  | _ -> check_mat_exprs row_tail t1 env
        | _ -> matrix_error t1 t2
*)

(* Check IDs *)
and check_id name env =
  let vdecl =
    try
      lookup_var name env.scope
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name))
  in
    let t = vdecl.styp in
      Sast.Expr(Sast.Id(name), t)

(* Check assignments *)
and check_assign name e env =
  let vdecl =
    try
      lookup_var name env.scope
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name))
    in
      let e = check_expr env e in
        match e with
          Sast.Expr(_, t) -> 
            let t1 = vdecl.styp in
              if t = t1 then
                Sast.Assign(name, e)
              else
                raise (Except("Invalid assignment"))

and check_call name args env = 
  let fdecl =
    try
      lookup_func name args env
    with Not_found ->
       raise (Except("Undeclared function or function signature mismatch"))

(* Checks unary operators *)
and check_unop op e env =
  let e = check_expr env e in 
    match e with
      Sast.Expr(_, t) ->
        (match op with
          Ast.Not ->
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | _ ->  unop_error op)
          | Ast.Re ->
            (match t with
              Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Im -> 
            (match t with
              Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Norm ->
            (match t with
              Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | Sast.Qub_bra -> Sast.Expr(Sast.Unop(op, e), Sast.Qub_bra)
              | Sast.Qub_ket -> Sast.Expr(Sast.Unop(op, e), Sast.Qub_ket)
              | _ ->  unop_error op)
          | Ast.Trans ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Det ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Adj ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Conj ->
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Unit -> 
            (match t with
              Ast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Sin -> 
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | Ast.Float -> Sast.Expr(Sast.Unop(op, e), Sast.Float)
              | Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Cos -> 
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | Ast.Float -> Sast.Expr(Sast.Unop(op, e), Sast.Float)
              | Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Tan -> 
            (match t with
              Ast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | Ast.Float -> Sast.Expr(Sast.Unop(op, e), Sast.Float)
              | Ast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op))

(* Checks binary operators *)
and check_binop e1 op e2 env =
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
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                        | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Ast.Float -> 
                      (match t2 with
                        Ast.Int | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Ast.Comp -> 
                      (match t2 with
                        Ast.Int | Ast.Float | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Ast.Mat ->
                      (match t2 with
                        Ast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Mat)
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
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                        | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Ast.Float -> 
                      (match t2 with
                        Ast.Int | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Ast.Comp -> 
                      (match t2 with
                        Ast.Int | Ast.Float | Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | _ -> binop_error op)
                | Ast.Tens -> 
                  (match t1 with
                    Ast.Mat ->
                      (match t2 with
                        Ast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Mat)
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
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                       | _ -> binop_error op)
                  | Ast.Float -> 
                      (match t2 with
                        Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | _ -> binop_error op)
                  | Ast.Comp -> 
                      (match t2 with
                        Ast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | _ -> binop_error op)
                  | _ -> binop_error op)
                | Ast.Or | Ast.And | Ast.Xor -> 
                  (match t1 with
                    Ast.Int ->
                      (match t2 with
                        Ast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                        | Ast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        |  _ -> binop_error op)
                    | _ -> binop_error op)))

and check_stmt env = function
    Ast.Expr(e) -> check_expr e env
    | Ast.Block(l) -> check_block l env
    | Ast.If(e, s) -> check_if e s env
    | Ast.For(e1, e2, e3, e4, s) -> check_for e1 e2 e3 e4 s env
    | Ast.While(e, s) -> e s env

and add_fdecl fdecl env =
  let sfdecl = fdecl_to_sdecl in 
    fdecl :: env.functions;


let fdecl_to_sdecl fdecl = 
  {
    sret_type = fdecl.ret_type;
    sret_name = 
  }


and sfunc_decl = 
  {
    sret_type : sdata_type;
    sret_name : string;
    sfunc_name : string;
    sformal_params : svar_decl list;
    slocals : svar_decl list;
    sbody : sstmt list;
  }

let rec check_locals locals val env = match locals with
  [] -> false
  | _ -> if (List.exists (fun x -> x = v) l)
           then true
         else
           check_locals (List.tl l) (List.hd l)

let rec check_fparams fparams val env = match fparams with
  [] -> false
  | _ -> if (List.exists (fun x -> x = v) l)
           then true
         else
           check_fparams (List.tl l) (List.hd l)

and rec check_fdecl name env =  
  try
    List.find (fun fdecl -> name = fdecl.sfunc_name) scope.variables; true;
  with Not_found -> false

and check_function fdecl env = 
  let fail = check_fdecl fdecl.func_name env in match fail with
    true -> raise program_error fdecl.name
    | false ->
        let l = fdecl.formal_params in
          let fail = check_fparams (List.tl l) (List.hd l) env in match fail with
            true -> raise program_error fdecl.name
            | false ->
                let l = fdecl.locals in
                  let fail = check_locals (List.tl l) (List.hd l) in match fail with
                    true -> raise program_error fdecl.name
                    | false ->  add_fdecl fdecl env

and check_vdecl vdecl env

  let vdecl =
    try
      lookup_var name env.scope
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name))
  in
    let t = vdecl.styp in
      Sast.Expr(Sast.Id(name), t)

  Ast.Lit_int(i) -> Sast.Expr(Sast.Lit_int(i), Sast.Int)

let check_program fdecls =
  let env = root_environment in
    List.map (check_functions fdecls env);