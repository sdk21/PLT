type row = float list

type matrix = row list

type lvalue =
    Ident of string
  | VAccess of string * expr
  | MAccess of string * expr * expr

and expr =
    NumLit of float
  | MatLit of matrix
  | StrLit of string
  | BoolLit of bool
  | LValue of lvalue
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Power of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Le of expr * expr
  | Ge of expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Trans of expr
  | SizeRows of expr
  | SizeCols of expr

type datatype =
    Scalar
  | String
  | Matrix
  | Boolean

type stmt =
    Assign of lvalue * expr
  | If of expr * stmt
  | IfElse of expr * stmt * stmt
  | While of expr * stmt
  | Print of expr
  | Dim of string * expr * expr
  | Decl of datatype * string
  | DeclInit of datatype * string * expr
  | StmtList of stmt list

type prgm = stmt list
