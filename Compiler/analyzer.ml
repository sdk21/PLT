(* Semantic Analyzer 
	- Consumes abstract syntax tree from parser
	- Produces semantically Analyzed Syntax Tree for compiler
*)

open Ast
open Sast

(***************
 * Environment *
***************)

type symbol_table =
  { parent : symbol_table option;
    ret_typ : Sast.sdata_type;
    ret_nam : string;
    func_nam : string;
    mutable formal_param : svar_decl list;
    mutable local : svar_decl list; }

type environment =
  { scope : symbol_table;
    mutable functions : Sast.sfunc_decl list; }

let root_symbol_table =
  { parent = None;
    ret_typ = Sast.Void;
    ret_nam = "";
    func_nam = "";
    formal_param = [];
    local = []; }

let root_environment = 
  { scope = root_symbol_table;
    functions = []; }

(**************
 * Exceptions *
**************)

exception Except of string

let matrix_error t1 t2 = match t2 with
  0 -> raise (Except("Invalid type in matrix"))
  | _ -> raise (Except("Type mismatch in matrix"))

let qub_error t = match t with
  1 -> raise (Except("Invalid use of <expr|"))
  | 0 -> raise (Except("Invalid use of |expr>"))
  | _ -> raise (Except("Invalid use qubits"))

let var_error s =
  raise (Except("Invalid use of a variable: " ^ s ^ " was not declared" ))

let var_decl_error s =
  raise (Except("Invalid variable declaration: " ^ s ^ " was already declared" ))

let func_decl_error s =
  raise (Except("Invalid function declaration: " ^ s ^ " was already declared" ))

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

let expr_error t = match t with
  _ -> raise (Except("Invalid expression"))

let call_error t = match t with
  _ -> raise (Except("Undeclared function or function signature mismatch"))

let if_error t = match t with
  _ -> raise (Except("Invalid use of 'if'"))

let for_error t = match t with
  _ -> raise (Except("Invalid use of 'for'"))

let while_error t = match t with
  _ -> raise (Except("Invalid use of 'while'"))

(*********************
 * Utility Functions *
*********************)

let var_exists name scope =
  let formal =
    List.exists (fun vdecl -> name = vdecl.sname) scope.formal_param
  in match formal with
  true -> true
  | false ->
      let local =
        List.exists (fun vdecl -> name = vdecl.sname) scope.local
      in
        local

let func_exists name env =
  List.exists (fun fdecl -> name = fdecl.sfunc_name) env.functions

let rec lookup_var name scope =
  let vdecl_found = 
    try
      List.find (fun vdecl -> name = vdecl.sname) scope.formal_param
    with Not_found ->
      try
        List.find (fun vdecl -> name = vdecl.sname) scope.local
      with Not_found ->
        match scope.parent with
          Some(parent) -> lookup_var name parent
          | _ -> raise Not_found
    in
      vdecl_found

let lookup_func name env =
  let fdecl_found = 
    try
      List.find (fun fdecl -> name = fdecl.sfunc_name) env.functions
    with Not_found -> raise Not_found
  in
    fdecl_found

(**********
 * Checks *
**********)

let rec check_qub_expr i =
  let r = i mod 10 in
   if (r = 0 || r = 1)
    then
     let i = i / 10 in
       if (i != 0)
        then
         check_qub_expr i
       else 1
    else 0

and check_qub i t =
  if (check_qub_expr i = 1)
    then
      (match t with
          0 -> Sast.Expr(Sast.Lit_qub(i), Sast.Qubb)
        | 1 -> Sast.Expr(Sast.Lit_qub(i), Sast.Qubk)
        | _ -> qub_error 2)
  else
    qub_error t

(* Checks matricies
and check_mat l env = 
  List.map (fun mat_expr -> mat_expr env) l;

(* Checks matrix expressions helper *)
and check_mat_exprs_helper l env =
  let e = (List.hd l) in
    let Sast.Expr(_, t) = check_expr e env in 
      match t with
        Sast.Int -> check_mat_exprs l Sast.Int env
        | Sast.Float -> check_mat_exprs l Sast.Float env
        | Sast.Comp -> check_mat_exprs l Sast.Comp env
        | _ -> matrix_error t 0

(* Checks matrix expressions *)
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

and check_id name env =
  let vdecl =
    try
      lookup_var name env.scope
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name))
  in
    let typ = vdecl.styp in
      Sast.Expr(Sast.Id(name), typ)

and check_unop op e env =
  let e = check_expr env e in 
    match e with
      Sast.Expr(q, t) ->
        (match op with
          Ast.Neg ->
            (match t with
              Sast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | Sast.Float -> Sast.Expr(Sast.Unop(op, e), Sast.Float)
              | Sast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ -> unop_error op)
          | Ast.Not ->
            (match t with
              Sast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | _ ->  unop_error op)
          | Ast.Re ->
            (match t with
              Sast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Im -> 
            (match t with
              Sast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Norm ->
            (match t with
              Sast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | Sast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | Sast.Qubb -> Sast.Expr(Sast.Unop(op, e), Sast.Qubb)
              | Sast.Qubk -> Sast.Expr(Sast.Unop(op, e), Sast.Qubk)
              | _ ->  unop_error op)
          | Ast.Trans ->
            (match t with
              Sast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Det ->
            (match t with
              Sast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Adj ->
            (match t with
              Sast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Conj ->
            (match t with
              Sast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Unit -> 
            (match t with
              Sast.Mat -> Sast.Expr(Sast.Unop(op, e), Sast.Mat)
              | _ ->  unop_error op)
          | Ast.Sin -> 
            (match t with
              Sast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | Sast.Float -> Sast.Expr(Sast.Unop(op, e), Sast.Float)
              | Sast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Cos -> 
            (match t with
              Sast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | Sast.Float -> Sast.Expr(Sast.Unop(op, e), Sast.Float)
              | Sast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op)
          | Ast.Tan -> 
            (match t with
              Sast.Int -> Sast.Expr(Sast.Unop(op, e), Sast.Int)
              | Sast.Float -> Sast.Expr(Sast.Unop(op, e), Sast.Float)
              | Sast.Comp -> Sast.Expr(Sast.Unop(op, e), Sast.Comp)
              | _ ->  unop_error op))

and check_binop e1 op e2 env =
    let e1 = check_expr env e1 and e2 = check_expr env e2 in
      match e1 with
        Sast.Expr(_, t1) ->
          (match e2 with
            Sast.Expr(_, t2) ->
              (match op with
                Ast.Add | Ast.Sub | Ast.Mult | Ast.Div ->
                  (match t1 with
                    Sast.Int -> 
                      (match t2 with
                        Sast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                        | Sast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Sast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Sast.Float -> 
                      (match t2 with
                        Sast.Int | Sast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Sast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Sast.Comp -> 
                      (match t2 with
                        Sast.Int | Sast.Float | Sast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Sast.Mat ->
                      (match t2 with
                        Sast.Mat -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Mat)
                        | _ -> binop_error op)
                    | Sast.Qubb ->
                      (match t2 with 
                        Sast.Qubb -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qubb)
                       |  _ -> binop_error op)
                    | Sast.Qubk ->
                      (match t2 with 
                        Sast.Qubk -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qubk)
                       |  _ -> binop_error op)
                    | _ -> binop_error op)
                | Ast.Tens ->
                  (match t1 with
                    Sast.Mat ->
                      (match t2 with
                        Sast.Mat->Sast.Expr(Sast.Binop(e1, op, e2), Sast.Mat)
                        | _ -> binop_error op)
                    | Sast.Qubb -> 
                      (match t2 with
                        Sast.Qubb -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qubb)
                        | _ -> binop_error op)
                    | Sast.Qubk ->
                      (match t2 with
                        Sast.Qubk -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Qubk)
                        | _ -> binop_error op)
                    | _ -> binop_error op)
                | Ast.Mod | Ast.Expn ->
                  (match t1 with
                    Sast.Int -> 
                      (match t2 with
                        Sast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                        | Sast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Sast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Sast.Float -> 
                      (match t2 with
                        Sast.Int | Sast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                        | Sast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | Sast.Comp -> 
                      (match t2 with
                        Sast.Int | Sast.Float | Sast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                        | _ -> binop_error op)
                    | _ -> binop_error op)
                | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq ->
                  (match t1 with
                    Sast.Int ->
                      (match t2 with
                        Sast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                        | _ -> binop_error op)
                    | Sast.Float ->
                        (match t2 with
                          Sast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                          | _ -> binop_error op)
                    | Sast.Comp ->
                      (match t2 with 
                        Sast.Comp -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Comp)
                       | _ -> binop_error op)  
                    | _ -> binop_error op)
                  | Ast.Or | Ast.And | Ast.Xor -> 
                    (match t1 with
                      Sast.Int ->
                        (match t2 with
                          Sast.Int -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Int)
                          | Sast.Float -> Sast.Expr(Sast.Binop(e1, op, e2), Sast.Float)
                          |  _ -> binop_error op)
                      | _ -> binop_error op)))

and check_assign name e env =
  let vdecl =
    try
      lookup_var name env.scope
     with Not_found ->
       raise (Except("Undeclared identifier: " ^ name))
    in
      let e = check_expr env e in
        match e with
          Sast.Expr(_, t1) -> 
            let t2 = vdecl.styp in
              if (t1 = t2)
                then Sast.Expr(Sast.Assign(name, e), t1)
              else
                raise (Except("Invalid assignment"))

and check_call_params formal_params params =
  if ((List.length formal_params) = 0)
    then true
  else
    let fdecl_arg =
      List.hd formal_params
    in
      let param = match (List.hd params) with
        Sast.Expr(_, t) -> t
      in
        if (fdecl_arg.styp = param)
          then check_call_params (List.tl formal_params) (List.tl params)
        else false

and check_call name params env = 
  let fdecl =
    try
      lookup_func name env
    with Not_found -> call_error 1
  in
    let params =
      List.map (check_expr env) params
    in
      if ((List.length fdecl.sformal_params) != (List.length params))
        then call_error 1
      else
        if ((check_call_params fdecl.sformal_params params) = true)
          then Sast.Expr(Sast.Call(name, params), fdecl.sret_typ)
        else
          call_error 1

and check_expr env = function
  Ast.Lit_int(i) -> Sast.Expr(Sast.Lit_int(i), Sast.Int)
  | Ast.Lit_float(f) -> Sast.Expr(Sast.Lit_float(f), Sast.Float)
  | Ast.Lit_comp(f1, f2) -> Sast.Expr(Sast.Lit_comp(f1, f2), Sast.Comp)
  | Ast.Lit_qub(i, t) -> check_qub i t
  | Ast.Id(s) -> check_id s env
  | Ast.Unop(op, e) -> check_unop op e env
  | Ast.Binop(e1, op, e2) -> check_binop e1 op e2 env
  | Ast.Assign(s, e) -> check_assign s e env
  | Ast.Call(s, l) -> check_call s l env
  | _ -> expr_error 1

and check_block stmts env =
    let sstmts =
      List.map (fun stmt -> check_stmt env stmt) stmts
    in
      Sast.Block(sstmts)

and check_if e s env =
    let se =
      check_expr env e
    in
      match se with
        Sast.Expr(_, Sast.Int) ->
          let ss =
            check_stmt env s
          in
            Sast.If(se, ss)
        | _ -> if_error 1

and check_for e1 e2 e3 e4 s env =
  let se1 =
    check_expr env e1
  in
    match se1 with
      Sast.Expr(Sast.Id(_), Sast.Int) ->
        let se2 =
          check_expr env e2
        in
          (match se2 with
            Sast.Expr(_, Sast.Int) ->
              let se3 =
                check_expr env e3
              in
                (match se3 with
                  Sast.Expr(_, Sast.Int) ->
                    let se4 =
                      check_expr env e4
                    in
                      (match se4 with
                        Sast.Expr(_, Sast.Int) ->
                        let ss =
                          check_stmt env s in
                            Sast.For(se1, se2, se3, se4, ss)
                      | _ -> for_error 1)
                | _ -> for_error 1)
          | _ -> for_error 1)
    | _ -> for_error 1

and check_while e s env =
  let se =
    check_expr env e
  in
    match se with
      Sast.Expr(Sast.Binop(_, op, _), Sast.Int) ->
        (match op with
          Ast.Eq | Ast.Neq | Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq ->
            let ss = check_stmt env s in
              Sast.While(se, ss)
          | _ -> while_error 1)
    | _ -> while_error 1

and check_stmt env = function
  Ast.Expr(e) -> Sast.Sexpr(check_expr env e)
  | Ast.Block(l) -> check_block l env
  | Ast.If(e, s) -> check_if e s env
  | Ast.For(e1, e2, e3, e4, s) -> check_for e1 e2 e3 e4 s env
  | Ast.While(e, s) -> check_while e s env

and vdecl_to_sdecl vdecl =
    match vdecl.typ with
      Ast.Int -> { styp = Sast.Int; sname = vdecl.name; }
      | Ast.Float -> { styp = Sast.Float; sname = vdecl.name; }
      | Ast.Comp -> { styp = Sast.Comp; sname = vdecl.name; }
      | Ast.Mat -> { styp = Sast.Mat; sname = vdecl.name; }
      | Ast.Qubb -> { styp = Sast.Qubb; sname = vdecl.name; }
      | Ast.Qubk -> { styp = Sast.Qubk; sname = vdecl.name; }

and formal_to_sformal scope formal_param  =
  let found =
    var_exists formal_param.name scope
  in
    match found with
      true -> var_decl_error formal_param.name
      | false ->
          let sdecl = 
            vdecl_to_sdecl formal_param
          in
            let new_formals = 
              sdecl :: scope.formal_param
            in
              let new_scope =
                { parent = scope.parent;
                  ret_typ = scope.ret_typ;
                  ret_nam = scope.ret_nam;
                  func_nam = scope.func_nam;
                  formal_param = new_formals;
                  local = scope.local; }
              in
                new_scope

and formals_to_sformals scope formal_params =
  let new_scope = 
    List.fold_left formal_to_sformal scope (List.rev formal_params)
  in
    new_scope

and local_to_slocal scope local =
  let found =
    var_exists local.name scope
  in
    match found with
      true -> var_decl_error local.name
      | false ->
          let sdecl = 
            vdecl_to_sdecl local
          in
            let new_locals = 
              sdecl :: scope.local
            in
              let new_scope =
                { parent = scope.parent;
                  ret_typ = scope.ret_typ;
                  ret_nam = scope.ret_nam;
                  func_nam = scope.func_nam;
                  formal_param = scope.formal_param;
                  local = new_locals; }
              in
                new_scope

and locals_to_slocals scope locals =
  let new_scope = 
    List.fold_left local_to_slocal scope (List.rev locals)
  in
    new_scope

and ret_to_sret scope ret_typ =
  let sret_typ = 
    match ret_typ with
      Ast.Int -> Sast.Int
      | Ast.Float -> Sast.Float
      | Ast.Comp -> Sast.Comp
      | Ast.Mat -> Sast.Mat
      | Ast.Qubb -> Sast.Qubb
      | Ast.Qubk -> Sast.Qubk
  in
    let new_scope =
      { parent = scope.parent;
        ret_typ = sret_typ;
        ret_nam = scope.ret_nam;
        func_nam = scope.func_nam;
        formal_param = scope.formal_param;
        local = scope.local; }
    in
      new_scope

and rname_to_srname scope ret_name =
    let new_scope =
      { parent = scope.parent;
        ret_typ = scope.ret_typ;
        ret_nam = ret_name;
        func_nam = scope.func_nam;
        formal_param = scope.formal_param;
        local = scope.local; }
    in
      new_scope

and fname_to_sfname scope func_name =
    let new_scope =
      { parent = scope.parent;
        ret_typ = scope.ret_typ;
        ret_nam = scope.ret_nam;
        func_nam = func_name;
        formal_param = scope.formal_param;
        local = scope.local; }
    in
      new_scope

and fdecl_to_sdecl fdecl env = 
  let new_scope = 
    formals_to_sformals env.scope fdecl.formal_params
  in
    let new_scope =
      locals_to_slocals new_scope fdecl.locals
    in
      let new_scope =
        ret_to_sret new_scope fdecl.ret_typ
      in
        let new_scope =
          rname_to_srname new_scope fdecl.ret_name
        in
          let new_scope =
            fname_to_sfname new_scope fdecl.func_name
          in
            let new_env =
              { scope = new_scope; functions = env.functions; }
            in
              let stmts =
                List.map (fun stmt -> check_stmt new_env stmt) fdecl.body
              in
                { sret_typ = new_scope.ret_typ;
                  sret_name = new_scope.ret_nam;
                  sfunc_name = new_scope.func_nam;
                  sformal_params = new_scope.formal_param;
                  slocals = new_scope.local;
                  sbody = stmts; }

and check_function env fdecl =
  let found =
    func_exists fdecl.func_name env
  in
    match found with
      true -> func_decl_error fdecl.func_name
      | false ->
          let sfdecl =
            fdecl_to_sdecl fdecl env
          in
            let new_env =
              { scope = env.scope;
                functions = sfdecl :: env.functions; }
            in
              new_env

and check_program fdecls =
  let env =
    List.fold_left check_function root_environment (List.rev fdecls)
  in
    env.functions