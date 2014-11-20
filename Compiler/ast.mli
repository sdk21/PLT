(* Abstract Syntax Tree (AST)
    - Produced by parser
    - Consumed by semantic analyzer
*)

(* Elementary Data Types *)
type data_type =
    Int
  | Float
  | Comp
  | Qub
  | Mat

(* Unary Operators *) 
type un_op =
  Not
  | Re
  | Im
  | Norm
  | Trans
  | Det
  | Adj
  | Conj
  | Unit
  | Sin
  | Cos
  | Tan

(* Binary Operators *)
type bi_op =
  Add
  | Sub
  | Mult
  | Div
  | Mod
  | Expn
  | Tens
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Or
  | And
  | Xor

(* Expressions *)
type expr =
  Lit_int of int
  | Lit_float of float
  | Comp of float * float
  | Qub_bra of expr
  | Qub_ket of expr
  | Mat of expr list list
  | Id of string
  | Unop of un_op * expr
  | Binop of expr * bi_op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr 

(* Statements *)
type stmt =
  Expr of expr
  | Block of stmt list
  | If of expr * stmt
  | For of expr * expr * expr * expr * stmt
  | While of expr * stmt
 
(* Variables Declaration *)
type var_decl = 
  { 
    var_type : data_type;
    var_name : string;
  }

(* Function Declaration *)
type func_decl = 
  {
    ret_type : data_type;
    ret_name : string;
    func_name : string;
    formal_params : var_decl list;
    locals : var_decl list;
    body : stmt list;
  }

(* Program *)
type program =
  var_decl list * func_decl list


(* ================
     PRINT AST
================= *)

(* 
let string_of_word string_of = function 
    Some(x) -> string_of x 
  | None -> ""


 (* Need to work on expr and stmts *)
let rec string_of_expr = function
    Lit_int(n) -> string_of_int n 
  | Lit_float(n) -> string_of_float n
  | Comp(f1,f2) ->  of float * float
  | Qub_bra of expr
  | Qub_ket of expr
  | Mat of expr list list
  | Id of string
  | Unop of un_op * expr
  | Binop of expr * bi_op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr 


let string_of_stmt = function
  Expr of expr
  | Block of stmt list
  | If of expr * stmt
  | For of expr * expr * expr * expr * stmt
  | While of expr * stmt
 

(* method for printing variable decls *)  
let string_of_var_decl var_decl = 
  "var_name: " ^ var_decl.var_name ^ "\nvar_type: " ^ var_decl.var_type ^ "\n"
  
(* method for printing function decls *)    
let string_of_fdecl fdecl =
  "\nfreturn: " ^ fdecl.ret_type ^ "\nfname: "  ^ fdecl.func_name ^  "(" ^
  String.concat " " (List.map string_of_var_decl fdecl.formal_params) ^ ")\n{\n" ^
  String.concat " " (List.map string_of_var_decl fdecl.locals) ^ " "^
  String.concat " " (List.map string_of_stmt fdecl.body) ^
  "}"


(* method for printing program - list of var_decl and func_decl *)  
let string_of_program (vars, funcs) = 
  "VARS: \n" ^ String.concat " " (List.map string_of_var_decl vars) ^ " \n\nFUNCTIONS: " ^
  String.concat "\n" (List.map string_of_fdecl funcs)


*)  