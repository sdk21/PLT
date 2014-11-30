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
    #include <Eigen>; 
    #include <stdio>;
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
    and cppLocals = cppLocalVar func.slocals
    and cppFBody = cppStmt func.sstmt in
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


and cppStmt = function
    Sexpr(sexpr) -> writeExpr sexpr
    | Block (stmtlist) -> 
            List.fold_left (fun a b -> a ^ (cppStmt b)) "" stmtlist    
    | If(expr , stmt) -> writeIfStmt expr stmt
    | For(var,init, final, increment, stmt) -> 
            writeForStmt var init final increment stmt
    | While(expr, stmt) -> writeWhileStmt expr stmt  



and writeWhileStmt expr stmt = 
let condString = writeCondition expr 
  and stmtString = writeStmts stmt in 
    sprintf "while (%s)\n%s\n" condString stmtString



(* For generating statements *)
and writeStmts stmts = match stmts with
Sast.Sexpr(sexpr) -> writeExpr expr ^ ";\n"
  | Sast.Block(sstmt list) -> writeStmtBlock sstmt list
  | Sast.If(expr_wrapper * sstmt) -> writeIfStmt expr_wrapper sstmt
  | Sast.For(expr_wrapper * expr_wrapper * expr_wrapper * expr_wrapper * sstmt) 
     -> writeForStmt expr_wrapper sstmt
  | Sast.While(expr_wrapper * sstmt) -> writeWhileStmt expr_wrapper sstmt


and writeStmtBlock sstmtl = 
let slist = List.fold_left (fun output element ->
    let stmt = writeStmts  element in
    output ^ stmt ^ "\n") "" slist in
    "\n{\n" ^ slist ^ "}\n"
  



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



