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
  | Lit_com of expr * expr
  | Lit_qub of expr
  | Lit_mat of expr
  | Id of string
  | Unop of un_op * expr
  | Binop of expr * bi_op *expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr 

(* Statements *)
type stmt =
  Block of stmt list
  | Expr of expr
  | Return of expr
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | If of expr * stmt

(* Variables Declaration *)
type var_decl = 
  { 
    var_type : data_type;
    var_name : string;
  }

(* Function Declaration *)
type func_decl = 
  {
    func_type : data_type;
    func_name : string;
    args : var_decl list;
    locals : var_decl list;
    body : stmt list;
  }


type program =
  var_decl list * func_decl list
