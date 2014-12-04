(* Semantically Analyzed Syntax Tree
    - Produced by semantic analyzer
    - Consumed by compiler
*)

open Ast

type sdata_type =
    Int
  | Float
  | Comp
  | Mat
  | Mati
  | Matf
  | Matc
  | Qubb
  | Qubk
  | Void

type expr_wrapper = 
    Expr of sexpr * sdata_type

and  sexpr =
    Lit_int of int
  | Lit_float of float
  | Lit_comp of float * float
  | Lit_qub of int
  | Lit_qubb of string * int
  | Lit_qubk of string * int
  | Mat of expr_wrapper list list
  | Id of string
  | Unop of Ast.un_op * expr_wrapper
  | Binop of expr_wrapper * Ast.bi_op * expr_wrapper
  | Assign of string * expr_wrapper
  | Call of string * expr_wrapper list
  | Noexpr

and sstmt =
    Sexpr of expr_wrapper
  | Block of sstmt list
  | If of expr_wrapper * sstmt
  | For of expr_wrapper * expr_wrapper * expr_wrapper * expr_wrapper * sstmt
  | While of expr_wrapper * sstmt
 
and svar_decl = 
  { 
    styp : sdata_type;
    sname : string;
  }

and sfunc_decl = 
  {
    sret_typ : sdata_type;
    sret_name : string;
    sfunc_name : string;
    sformal_params : svar_decl list;
    slocals : svar_decl list;
    sbody : sstmt list;
  }

type sprogram =
  sfunc_decl list

(* Prety Printer *)
let rec string_of_unop op e =
  (match op with
  Neg -> " -"
  | Not -> " ! "
  | Re -> " Re "
  | Im -> " Im "
  | Norm -> " Norm "
  | Trans -> " Trans "
  | Det -> " Det "
  | Adj -> " Adj "
  | Conj -> " Conj "
  | Unit -> " Unit "
  | Sin -> " Sin "
  | Cos -> " Cos "
  | Tan -> " Tan ") ^ string_of_expr_wrapper e

and string_of_binop e1 op e2 =
  string_of_expr_wrapper e1 ^ 
    (match op with 
      Add -> " + "    | Sub -> " - "     | Mult -> " * " 
      | Div -> " / "    | Mod -> " % "     | Expn -> " ^ " | Tens -> " @ "
      | Eq-> " == "     | Neq -> " != "    | Lt -> " < "
      | Leq -> " <= "   | Gt -> " > "      | Geq -> " >= "
      | Xor -> " XOR "  | And -> " && "    | Or -> " || ") ^ string_of_expr_wrapper e2

and string_of_mat l =
  let row_strs = 
    List.map string_of_row l
  in
    "[" ^ String.concat "" row_strs ^ "]"

and string_of_row r =
  let row_str = 
    String.concat "," (List.map string_of_expr_wrapper r)
  in
    "(" ^ row_str ^ ")"

and string_of_sexpr = function
    Lit_int(i) -> string_of_int i
    | Lit_float(f) -> string_of_float f
    | Lit_comp(f1, f2) -> string_of_float f1 ^ " + " ^ string_of_float f2 ^ "i"
    | Lit_qub(i) -> string_of_int i
    | Mat(l) ->  string_of_mat l
    | Id(s) -> s
    | Unop(op, e) -> string_of_unop op e
    | Binop(e1, op, e2) -> string_of_binop e1 op e2
    | Assign(name, e) -> name ^ " = " ^ string_of_expr_wrapper e
    | Call(name, params) -> "Calling " ^ name ^ " on " ^ string_of_sexprs params
    | Noexpr -> "noexpr"

and string_of_expr_wrapper w =
  let sexpr =
    match w with
        Expr(Lit_int(i), Int) -> Lit_int(i)
      | Expr(Lit_float(f), Float) -> Lit_float(f)
      | Expr(Lit_comp(f1, f2), Comp) -> Lit_comp(f1, f2)
      | Expr(Mat(l), Mati) -> Mat(l)
      | Expr(Mat(l), Matf) -> Mat(l)
      | Expr(Mat(l), Matc) -> Mat(l)
      | Expr(Id(name), typ) -> Id(name)
      | Expr(Unop(op, e), _) -> Unop(op, e) 
      | Expr(Binop(e1, op, e2), _) -> Binop(e1, op, e2)
      | Expr(Assign(name, e), t1) -> Assign(name, e)
      | Expr(Call(name, params), _) -> Call(name, params)
      | Expr(Lit_qub(i), _) -> Lit_qub(i)
      | _ -> Noexpr
    in
      string_of_sexpr sexpr

and string_of_svar_decl svar_decl = 
  "svdecl: styp: " ^ 
    (match svar_decl.styp with
      Int -> "int," ^ " name: " ^ svar_decl.sname ^ "  "
    | Float -> "float," ^ " name: " ^ svar_decl.sname ^ "  "
    | Comp -> "comp," ^ " name: " ^ svar_decl.sname ^ "  "
    | Mat -> "mat," ^ " name: " ^ svar_decl.sname ^ "  "
    | Qubb -> "qubb," ^ " name: " ^ svar_decl.sname ^ "  "
    | Qubk -> "qubk," ^ " name: " ^ svar_decl.sname ^ "  "
    | _ -> "")

and string_of_sexprs e = 
  String.concat "\n" (List.map string_of_expr_wrapper e)

and string_of_sstmt = function
    Sexpr(e) -> string_of_expr_wrapper e ^ "\n"
  | Block(l) -> "{\n" ^ string_of_sstmts l ^ "\n}"
  | If(e,s) -> "If condition : " ^ string_of_expr_wrapper e ^ "\nstatement :\n" ^ string_of_sstmt s
  | For(e1, e2, e3, e4, s) -> "For args : " ^ string_of_expr_wrapper e1 ^ " " ^ string_of_expr_wrapper e2 ^ " "^ string_of_expr_wrapper e3 ^ 
                                 " "^ string_of_expr_wrapper e4 ^ "\nstatement :\n" ^ string_of_sstmt s 
  | While(e,s) -> "While condition : " ^ string_of_expr_wrapper e ^ "\nstatement : " ^ string_of_sstmt s

and string_of_sstmts sstmts = 
  String.concat "\n" (List.map string_of_sstmt sstmts) 

and string_of_sfdecl sfdecl =
  "\nsfdecl:\nsret_typ: " ^ 
    (match sfdecl.sret_typ with
      Int -> " int "
    | Float -> " float "
    | Comp -> " comp "
    | Mat -> " mat "
    | Qubb -> " qubb "
    | Qubk -> " qubk "
    | _ -> "") ^
      "\nsret_name: " ^ sfdecl.sret_name ^ "\nsfunc_name: "  ^ sfdecl.sfunc_name ^  "\n(" ^
        String.concat "" (List.map string_of_svar_decl sfdecl.sformal_params) ^ ")\n{\n" ^
          String.concat "" (List.map string_of_svar_decl sfdecl.slocals) ^ "\n" ^
            String.concat "" (List.map string_of_sstmt sfdecl.sbody) ^ "}"
 
and string_of_sprogram (l) = 
  "program:\n" ^ String.concat "\n" (List.map string_of_sfdecl l)
