open Sast
open Printf

(* type_of from Sast *)
let type_of (a : Sast.expr_wrapper) : Sast.t =
    match a with
    | Expr(_,t)-> t

let cpp_from_type (ty: Sast.t) : string =
    match ty with
    | Int -> "int"
    | Float -> "float"
    | Comp -> "complex"
    | Mat -> "Matrix"
    | Qub_bra -> "Matrix"
    | Qub_ket -> "Matrix"

let rec writeToFile fileName progString =
    let file = open_out (fileName ^ ".cpp") in
        fprintf file "%s" progString

and gen_program fileName prog =
    let cppString = writeCpp prog in
    let out = sprintf "
    #include <Eigen> #include <stdio> #include <cmath> #include <complex>
    %s
    " cppString in 
    writeToFile fileName out;
    out

and writeCpp funcList =
    let outStr = List.fold_left (fun a b -> a ^ (cpp_funcList b)) "" stmtList in
    sprintf "%s" outStr

and cpp_funcList func =
    let cppRtnType = cppReturnType func.sret_type
    and cppRtnValue = cppReturnValue func.sret_name
    and cppFName = func.sfunc_name
    and cppFParam = cppVarDecl func.sformal_params
    and cppFBody = cppStmt func.sbody in
    let cppfunc = sprintf "
    %s %s (%s){
        %s
        return %s
    }
    " cppRtnType cppFName cppFParam cppFBody cppRtnValue

and cppReturnType  = function

and cppReturnValue = function

and cppVarDecl = function

and cppExpr = function
  Binop(expr1, op, expr2, _) -> writeBinop expr1 op expr2
  (*
  | Lit_int(lit, _) -> 
  | Lit_float(flit, _) ->
  | Lit_comp(comlit, _) ->
  | Qub of expr_wrapper
  | Mat of expr_wrapper list list
  | Id(str) of string
  | Unop(op,  of Ast.un_op * expr_wrapper
  | Assign(name, expr) of string * expr_wrapper
  | Call of string * expr_wrapper list
  | Noexpr
  *)


and cppStmt = function 
      If(expr , stmt) -> writeIfStmt expr stmt
    | For(var,init, final, increment, stmt) -> 
            writeForStmt var init final increment stmt
    | While(expr, stmt) -> writeWhileStmt expr stmt  

and writeIfStmt expr stmt = 
	let cond = cppExpr expr 
	and body = writeCpp stmt in (*probably not right function call*)
	sprintf "
		if(%s) {
			%s
		} " cond body
	in 

and writeBinop expr1 op expr2 = 
    let e1 = cppExpr expr1 and e2 = cppExpr expr2 in 
    	let binopFunc e1 op e2 = match op with 
		Add 	-> sprintf "%s + %s" e1 e2
		| Sub 	-> sprintf "%s - %s" e1 e2
		| Mult 	-> sprintf "%s * %s" e1 e2
		| Div 	-> sprintf "%s / %s" e1 e2
		| Mod 	-> sprintf "%s % %s" e1 e2
		| Expn 	-> sprintf "pow(%s,%s)" e1 e2
		| Tens 	-> sprintf "tensor(%s, %s)" e1 e2
		| Eq 	-> sprintf "%s == %s" e1 e2
		| Neq 	-> sprintf "%s != %s" e1 e2
		| Lt 	-> sprintf "%s < %s" e1 e2
		| Gt 	-> sprintf "%s > %s" e1 e2
		| Leq 	-> sprintf "%s <= %s" e1 e2
		| Geq 	-> sprintf "%s >= %s" e1 e2
		| Or 	-> sprintf "%s || %s" e1 e2
		| And 	-> sprintf "%s && %s" e1 e2
		(*| Xor 	-> sprintf "%s ^ %s" e1 e2*)
	in binopFunc e1 op e2




