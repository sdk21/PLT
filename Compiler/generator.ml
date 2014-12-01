(*
open Sast
open Printf

(* type_of from Sast *)
let type_of (a : Sast.expr_wrapper) : Sast.sdata_type =
    match a with
    | Expr(_,t)-> t

let cpp_from_type (ty: Sast.sdata_type) : string =
    match ty with
    | Int -> "int"
    | Float -> "float"
    | Comp -> "complex"
    | Mat -> "Matrix"
    | Mat_int -> "Matrix"
    | Mat_float -> "Matrix"
    | Mat_comp -> "Matrix"
    | Qub -> "Matrix"
    | Qub_bra -> "Matrix"
    | Qub_ket -> "Matrix"

let rec writeToFile fileName progString =
    let file = open_out (fileName ^ ".cpp") in
        fprintf file "%s" progString

and gen_program fileName prog =
    let cppString = writeCpp prog in
    let out = sprintf "
    #include <Eigen/Dense>
    #include <cmath>
    #include <complex>
    #include <iostream>
    #include "../cpp/constants.h"
    #include "../cpp/tensorProduct.h"

    using namespace Eigen;
    using namespace std;
    %s
    " cppString in 
    writeToFile fileName out;
    out

and writeCpp funcList =
    let outStr = List.fold_left (fun a b -> a ^ (cpp_funcList b)) "" funcList in
    sprintf "%s" outStr

and cpp_funcList func =
    let cppRtnType = cppReturnType func.sret_type
    and cppRtnValue = cppReturnValue func.sret_name
    and cppFName = func.sfunc_name
    and cppFParam = cppVarDecl func.sformal_params
    and cppFBody = cppStmt func.sbody in
    and cppLocals = cppLocalVar func.slocals
    let cppfunc = sprintf "
    %s %s (%s){
        %s
        return  %s
    }
    " cppRtnType cppFName cppFParam cppFBody cppRtnValue

and cppReturnType  = function


and cppReturnValue = function

and cppVarDecl vardeclist =
   let varDecStr = List.fold_left (fun a b -> a ^ (cppVar b)) "" vardeclist in
   sprintf "%s" varDecStr 

and cppExpr = function
  Binop(expr1, op, expr2) -> writeBinop expr1 op expr2
  | Lit_int(lit) -> lit
  | Lit_float(flit) -> flit 
  | Lit_comp(comlit) -> comlit (* Not sure how to do this *)
  | Unop(op, expr) ->  writeUnop op expr



  (*
  | Qub of expr_wrapper
  | Mat of expr_wrapper list list
 *)
  | Id(str) -> str 
  | Assign(name, expr) ->  name  ^ cppExpr expr
 (* | Call of string * expr_wrapper list *)
  | Noexpr -> ""
  

(* For generating statements *)
and cppStmt stmts = match stmts with
Sast.Sexpr(sexpr) -> cppExpr expr ^ ";\n"  
  | Sast.Block(sstmt list) -> cppStmtBlock sstmt list
  | Sast.If(expr_wrapper * sstmt) -> writeIfStmt expr_wrapper sstmt
  | Sast.For(var,init, final, increment, stmt) -> 
            writeForStmt var init final increment stmt
  | Sast.While(expr_wrapper * sstmt) -> writeWhileStmt expr_wrapper sstmt


and cppStmtBlock sstmtl = 
let slist = List.fold_left (fun output element ->
    let stmt = cppStmt  element in
    output ^ stmt ^ "\n") "" slist in
    "\n{\n" ^ slist ^ "}\n"


(*
and writeIfStmt expr stmt = 
	let cond = cppExpr expr 
	and body = cppStmt stmt in (*probably not right function call*)
	sprintf "
		if(%s) {
			%s
		} " cond body
	*)

and writeWhileStmt expr stmt = 
let condString = cppExpr expr  
  and stmtString = cppStmt stmt in 
    sprintf "while (%s)\n%s\n" condString stmtString

and writeForStmt var init final increment stmt =
    let varname = var 
    and initvalue = string_of_int init
    and finalvalue = string_of_int final
    and incrementval = string_of_int increment
    and stmtbody = cppStmt stmt
    in
    sprintf "
    for (int %s = %s; %s < %s ; %s = %s + %s){
        %s
        }" varname initvaluevarname finalvalue varname varname incrementval stmtbody

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


and writeUnop op expr = 
    let exp = cppExpr expr in 
        let unopFunc op exp = match op with
        Neg     -> sprintf "  -%s" exp
        | Not   -> sprintf "  !(%s)" exp
        | Re    -> sprintf "  real(%s)" exp 	(* assumes exp is matrix*)
        | Im    -> sprintf "  imag(%s)" exp
        | Norm  -> sprintf "  norm(%s)" exp
        | Trans -> sprintf "  %s.transpose()" exp
        | Det   -> sprintf "  %s.determinant()" exp
        | Adj   -> sprintf "  %s.adjoint()" exp
        | Conj  -> sprintf "  %s.conjugate()" exp
        | Unit  -> sprintf "  %s" exp  		(* till here *)
        | Sin   -> sprintf "  sin((double)%s)" exp
        | Cos   -> sprintf "  cos((double)%s)" exp
        | Tan   -> sprintf "  tan((double)%s)" exp

    in unopFunc op exp
*)
