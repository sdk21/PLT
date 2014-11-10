(* Elementary Data Types *)
type data_type =
    Int
  | Float
  | Com
  | Qub
  | Mat

(* Unary Operators *) 
type un_op =
  Not
  | Re
  | Im
  | Norm
  | Unit
  | Trans
  | Det
  | Adj
  | Con
  | Sin
  | Cos
  | Tan
  | Exp

(* Binary Operators *)
type bi_op =
  Add
  | Sub
  | Mult
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Or
  | And
  | Xor
  | Tens

(* Expressions *)
type expr =
  Lit_int of int
  | Lit_float of float
  | Com of float * float
  | Qub_bra of expr
  | Qub_ket of expr
  | Mat of expr list list
  | Id of string
  | Unop of un_op * expr
  | Binop of expr * bi_op *expr
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

type program =
  var_decl list * func_decl list
