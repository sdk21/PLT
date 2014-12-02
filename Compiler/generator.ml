open Sast
open Printf
open String

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
(*
    | Mat_int -> "Matrix"
    | Mat_float -> "Matrix"
    | Mat_comp -> "Matrix"
    | Qub -> "Matrix"
    | Qub_bra -> "Matrix"
    | Qub_ket -> "Matrix"
*)
    | Qubb -> "Matrix"
    | Qubk -> "Matrix"

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
    #include \"../cpp/qlang.h\"

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
    let cppRtnType = cppReturnType func.sret_typ
    and cppRtnValue = cppReturnValue func.sret_name
    and cppFName = func.sfunc_name
    and cppFParam = cppVarDecl func.sformal_params
    and cppFBody = cppStmt func.sbody 
    and cppLocals = cppLocalVar func.slocals in
    sprintf "
    %s %s (%s){
	%s %s;
        %s
        return %s;
    }
    " cppRtnType cppFName cppFParam cppRtnType cppRtnValue cppFBody cppRtnValue

and cppReturnType rtntype  = cpp_from_type rtntype 

and cppReturnValue rtnval = rtnval


and cppVarDecl vardeclist =
   let varDecStr = List.fold_left (fun a b -> a ^ (cppVar b)) "" vardeclist in
   let varDectrun = String.sub varDecStr 0 ((String.length varDecStr)-1) in
   sprintf "%s" varDectrun

and cppVar var =
    let vartype = cpp_from_type var.styp in 
    sprintf " %s %s," vartype var.sname
   
and cppExpr = function
  Binop(expr1, op, expr2) -> writeBinop expr1 op expr2
  | Lit_int(lit) -> lit ^ " "
  | Lit_float(flit) -> flit ^ " "
  | Lit_comp(comlit) -> " (" ^ writeUnop Re comlit "," ^ writeUnop Im comlit ^ ") " (* Not sure how to do this *)
  | Unop(op, expr) ->  writeUnop op expr
  | Qub(expr) -> writeQubit expr
  | Mat (expr_wrap) -> writeMatrix expr_wrap
  | Id(str) -> str 
  | Assign(name, expr) ->  name  ^ " = " ^ cppExpr expr
  | Call(str,expr_wrapper) -> str ^  writeArgs expr_wrapper  
  | Noexpr -> ""

(* For generating statements *)
and cppStmt stmts = match stmts with
Sast.Sexpr(sexpr) -> cppExpr expr ^ ";\n"  
  | Sast.Block(sstmt) -> cppStmtBlock sstmt list
  | Sast.If(expr_wrapper , sstmt) -> writeIfStmt expr_wrapper sstmt
  | Sast.For(var,init, final, increment, stmt) -> 
            writeForStmt var init final increment stmt
  | Sast.While(expr_wrapper , sstmt) -> writeWhileStmt expr_wrapper sstmt


and cppStmtBlock sstmtl = 
let slist = List.fold_left (fun output element ->
    let stmt = cppStmt  element in
    output ^ stmt ^ "\n") "" slist in
    "\n{\n" ^ slist ^ "}\n"


and writeArgs argList =
let args = List.fold_left (fun output element ->
    let el = cppExpr element in 
    output ^ el ^ "," ) "" args in 
    "(" ^ String.sub args 0 ((String.length args)-1) ^ ") "


and writeIfStmt expr stmt = 
	let cond = cppExpr expr 
	and body = cppStmt stmt in (*probably not right function call*)
	sprintf "
		if(%s) {
			%s
		} " cond body

and writeWhileStmt expr stmt = 
let condString = cppExpr expr  
  and stmtString = cppStmt stmt in 
    sprintf "while (%s)\n%s\n" condString stmtString

and writeForStmt var init final increment stmt =
    let varname = cppExpr var 
    and initvalue = cppExpr init
    and finalvalue = cppExpr final
    and incrementval = cppExpr increment
    and stmtbody = cppStmt stmt
    in
    sprintf "
    for (int %s = %s; %s < %s ; %s = %s + %s){
        %s
        }" varname initvalue varname finalvalue varname varname incrementval stmtbody

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

and writeMatrix expr_wrap = 
    let matrixStr = List.fold_left (fun a b -> a ^ (writeRow b) ^ "\n") "" expr_wrap in
    let submatrix = String.sub matrixStr 0 ((String.length matrixStr)-2) in
    sprintf "%s\n;" submatrix 

and writeRow row_expr =
    let rowStr = List.fold_left (fun a b -> a ^ (cppExpr b) ^ "," ) row_expr in
    sprintf "%s" rowStr

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

(*probably doesn't work yet due to string format of expr*)
and writeQubit expr =
    let exp = cppExpr expr in
	sprintf "genQubit(%s)" exp
