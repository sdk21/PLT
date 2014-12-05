(* Abstract Syntax Tree (AST)
    - Produced by parser
    - Consumed by semantic analyzer
*)

(* Elementary Data Types *)
type data_type =
    Int
  | Float
  | Comp
  | Mat
  | Qubb
  | Qubk

(* Unary Operators *) 
type un_op =
  Neg
  | Not
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
  | Lit_comp of float * float
  | Lit_qub of string * int
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
  | Print of expr

(* Statement Lists *)
type stmt_list =
  stmt list
 
(* Variables Declaration *)
type var_decl = 
  { 
    typ : data_type;
    name : string;
  }

(* Function Declaration *)
type func_decl = 
  {
    ret_typ : data_type;
    ret_name : string;
    func_name : string;
    formal_params : var_decl list;
    locals : var_decl list;
    body : stmt list;
  }

(* Program *)
type program =
  func_decl list

(* Pretty Printer *)
let rec string_of_expr = function
    Lit_int(n) -> string_of_int n
  | Lit_float(n) -> string_of_float n
  | Lit_comp(f1,f2) -> string_of_float f1 ^ " + " ^ string_of_float f2 ^ "i"
  | Lit_qub(s,t) -> let typ = string_of_int t in (match typ with
                      "0" -> "Qub-bra of "^ s 
                    | _ -> "Qub-ket of "^ s)
  | Mat(l) ->  string_of_mat l
  | Id(s) -> s
  | Unop(un1,exp1) -> 
    (match un1 with
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
    | Tan -> " Tan ") ^ string_of_expr exp1 

  | Binop(ex1,binop,ex2) -> string_of_expr ex1 ^ 
    (match binop with 
      Add -> " + "    | Sub -> " - "     | Mult -> " * " 
    | Div -> " / "    | Mod -> " % "     | Expn -> " ^ " | Tens -> " @ "
    | Eq-> " == "     | Neq -> " != "    | Lt -> " < "
    | Leq -> " <= "   | Gt -> " > "      | Geq -> " >= "
    | Xor -> " XOR "  | And -> " && "    | Or -> " || ") ^ string_of_expr ex2
  | Assign(str,expr) -> str ^ " = " ^ string_of_expr expr
  | Call(str,expr_list) -> "Calling " ^ str ^ " on " ^string_of_exprs expr_list
  | Noexpr -> ""

and string_of_mat l =
  let row_strs = 
    List.map string_of_row l
  in
    "[" ^ String.concat "" row_strs ^ "]"

and string_of_row r =
  let row_str = 
    String.concat "," (List.map string_of_expr r)
  in
    "(" ^ row_str ^ ")"
  
and string_of_exprs exprs = 
  String.concat "\n" (List.map string_of_expr exprs)

and string_of_stmt = function
    Expr(exp) -> string_of_expr exp ^ "\n"
  | Block(stmt_list) -> "{\n" ^ string_of_stmts stmt_list ^ "\n}"
  | If(expr,stmt) -> "If condition : " ^ string_of_expr expr ^ "\nstatement :\n" ^ string_of_stmt stmt
  | For(ex1,ex2,ex3,ex4,stmt) -> "For args : " ^ string_of_expr ex1 ^ " " ^ string_of_expr ex2 ^ " "^ string_of_expr ex3 ^ 
                                 " "^ string_of_expr ex4 ^ "\nstatement :\n" ^ string_of_stmt stmt 
  | While(expr,stmt) -> "While condition : " ^ string_of_expr expr ^ "\nstatement : " ^ string_of_stmt stmt
  | Print(expr) -> "Print(" ^ string_of_expr expr ^")"

and string_of_stmts stmts = 
  String.concat "\n" (List.map string_of_stmt stmts) 

and string_of_var_decl var_decl = 
  "vdecl: typ: " ^ 
    (match var_decl.typ with
      Int -> "int," ^ " name: " ^ var_decl.name^ "  "
    | Float -> "float," ^ " name: " ^ var_decl.name^ "  "
    | Comp -> "comp," ^ " name: " ^ var_decl.name^ "  "
    | Mat -> "mat," ^ " name: " ^ var_decl.name^ "  "
    | Qubb -> "qubb," ^ " name: " ^ var_decl.name^ "  "
    | Qubk -> "qubk," ^ " name: " ^ var_decl.name^ "  ")
  
and string_of_fdecl fdecl =
  "\nfdecl:\nret_typ: " ^ 
    (match fdecl.ret_typ with
      Int -> " int "
    | Float -> " float "
    | Comp -> " comp "
    | Mat -> " mat "
    | Qubb -> " qubb "
    | Qubk -> " qubk ") ^
      "\nret_name: " ^ fdecl.ret_name ^ "\nfunc_name: "  ^ fdecl.func_name ^  "\n(" ^
        String.concat "" (List.map string_of_var_decl fdecl.formal_params) ^ ")\n{\n" ^
          String.concat "" (List.map string_of_var_decl fdecl.locals) ^ "\n" ^
            String.concat "" (List.map string_of_stmt fdecl.body) ^ "}"

and string_of_program (funcs) = 
  "program:\n" ^ String.concat "\n" (List.map string_of_fdecl funcs)