open Sast
open Printf
open String

(* type_of from Sast *)
let type_of (a : Sast.expr_wrapper) : Sast.sdata_type =
    match a with
    | Expr(_,t)-> t

let expr_of (a : Sast.expr_wrapper) : Sast.sexpr =
    match a with
    | Expr(e,_)-> e

let cpp_from_type (ty: Sast.sdata_type) : string =
    match ty with
    | Int -> "int"
    | Float -> "float"
    | Comp -> "complex"
    | Mat -> "Matrix"
    | Mati -> "Matrix"
    | Matf -> "Matrix"
    | Matc -> "Matrix"
    | Qubb -> "Matrix"
    | Qubk -> "Matrix"
    | Void -> " "

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
        %s" cppString in 
    writeToFile fileName out;

and writeCpp funcList =
    let outStr = List.fold_left (fun a b -> a ^ (cpp_funcList b)) "" funcList in
    sprintf "%s" outStr

and cpp_funcList func =
    let cppRtnType = cpp_from_type func.sret_typ
    and cppRtnValue = func.sret_name
    and cppFName = func.sfunc_name
    and cppFParam = cppVarDecl func.sformal_params ","
    and cppFBody = cppStmtList func.sbody 
    and cppLocals = cppVarDecl func.slocals ";\n\t" in
    sprintf "
    %s %s (%s){
	%s %s;
	%s 
        %s
        return %s;
    }
    " cppRtnType cppFName cppFParam cppRtnType cppRtnValue cppLocals cppFBody cppRtnValue

and cppVarDecl vardeclist delim =
   let varDecStr = List.fold_left (fun a b -> a ^ (cppVar b delim)) "" vardeclist in
   let varDectrun = String.sub varDecStr 0 ((String.length varDecStr)-1) in
   sprintf "%s " varDectrun

and cppVar var delim =
    let vartype = cpp_from_type var.styp in 
    sprintf " %s %s%s" vartype var.sname delim

and cppStmtList astmtlist =    
    let outStr = List.fold_left (fun a b -> a ^ (cppStmt b)) "" astmtlist in
    sprintf "%s" outStr

(* For generating statements *)
and cppStmt stmts = match stmts with
    Sast.Sexpr(expr_wrap) -> cppExpr (expr_of expr_wrap) ^ ";\n"  
  | Sast.Block(sstmt) -> cppStmtBlock sstmt
  | Sast.If(expr_wrap , sstmt) -> writeIfStmt (expr_of expr_wrap) sstmt
  | Sast.For(var,init, final, increment, stmt) -> 
            writeForStmt var init final increment stmt
  | Sast.While(expr_wrap , sstmt) -> writeWhileStmt (expr_of expr_wrap) sstmt

(* For generating expressions*)
and cppExpr expr = match expr with
   Lit_int(lit) -> string_of_int lit ^ " "
  | Lit_float(flit) -> string_of_float flit ^ " "
  | Lit_comp(re,im) -> " (" ^ string_of_float re ^ "," ^ string_of_float im  ^ ") " (* Not sure how to do this *)
  | Unop(op, expr) ->  writeUnop op expr
  | Binop(expr1, op, expr2) -> writeBinop expr1 op expr2
  | Lit_qub(expr) -> writeQubit expr
  | Mat (expr_wrap) -> writeMatrix expr_wrap
  | Id(str) -> str 
  | Assign(name, expr) ->  name  ^ " = " ^ cppExpr (expr_of expr)
  | Call(str,expr_wrap) -> str ^  ""  
  | Noexpr -> ""

and cppStmtBlock sstmtl = 
let slist = List.fold_left (fun output element ->
    let stmt = cppStmt  element in
    output ^ stmt ^ "\n") "" sstmtl in
    "\n{\n" ^ slist ^ "}\n"

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
    let varname = cppExpr (expr_of var) 
    and initvalue = cppExpr (expr_of init)
    and finalvalue = cppExpr (expr_of final)
    and incrementval = cppExpr (expr_of increment)
    and stmtbody = cppStmt stmt
    in
    sprintf "
    for (int %s = %s; %s < %s ; %s = %s + %s){
        %s
        }" varname initvalue varname finalvalue varname varname incrementval stmtbody

and writeBinop expr1 op expr2 = 
    let e1 = cppExpr (expr_of expr1) and e2 = cppExpr (expr_of expr2) in 
    	let binopFunc e1 op e2 = match op with 
		 Ast.Add 	-> sprintf "%s + %s" e1 e2
		| Ast.Sub 	-> sprintf "%s - %s" e1 e2
		| Ast.Mult 	-> sprintf "%s * %s" e1 e2
		| Ast.Div 	-> sprintf "%s / %s" e1 e2
		| Ast.Mod 	-> sprintf "%s ~ %s" e1 e2
		| Ast.Expn 	-> sprintf "pow(%s,%s)" e1 e2
		| Ast.Tens 	-> sprintf "tensor(%s, %s)" e1 e2
		| Ast.Eq 	-> sprintf "%s == %s" e1 e2
		| Ast.Neq 	-> sprintf "%s != %s" e1 e2
		| Ast.Lt 	-> sprintf "%s < %s" e1 e2
		| Ast.Gt 	-> sprintf "%s > %s" e1 e2
		| Ast.Leq 	-> sprintf "%s <= %s" e1 e2
		| Ast.Geq 	-> sprintf "%s >= %s" e1 e2
		| Ast.Or 	-> sprintf "%s || %s" e1 e2
		| Ast.And 	-> sprintf "%s && %s" e1 e2
		| Ast.Xor 	-> sprintf "%s ^ %s" e1 e2
	in binopFunc e1 op e2

and writeMatrix expr_wrap = 
    let matrixStr = List.fold_left (fun a b -> a ^ (writeRow b) ^ "\n") "" expr_wrap in
    let submatrix = String.sub matrixStr 0 ((String.length matrixStr)-2) in
    sprintf "%s" submatrix 

and writeRow row_expr =
    let rowStr = List.fold_left (fun a b -> a ^ (cppExpr (expr_of b)) ^ "," ) "" row_expr in
    sprintf "%s" rowStr

and writeUnop op expr = 
    let exp = cppExpr (expr_of expr) in 
        let unopFunc op exp = match op with
          Ast.Neg     -> sprintf "  -%s" exp
        | Ast.Not   -> sprintf "  !(%s)" exp
        | Ast.Re    -> sprintf "  real(%s)" exp 	(* assumes exp is matrix*)
        | Ast.Im    -> sprintf "  imag(%s)" exp
        | Ast.Norm  -> sprintf "  norm(%s)" exp
        | Ast.Trans -> sprintf "  %s.transpose()" exp
        | Ast.Det   -> sprintf "  %s.determinant()" exp
        | Ast.Adj   -> sprintf "  %s.adjoint()" exp
        | Ast.Conj  -> sprintf "  %s.conjugate()" exp
        | Ast.Unit  -> sprintf "  %s" exp  		(* till here *)
        | Ast.Sin   -> sprintf "  sin((double)%s)" exp
        | Ast.Cos   -> sprintf "  cos((double)%s)" exp
        | Ast.Tan   -> sprintf "  tan((double)%s)" exp
    in unopFunc op exp

(*probably doesn't work yet due to string format of expr*)
and writeQubit expr =
    let exp = string_of_int expr in
	sprintf "genQubit(%s)" exp
     
