type dataType =
	  Int
	| Float
	| IntRowVec
	| FloatRowVec
	| IntColVec
	| FloatColVec
	| IntMatrix
	| FloatMatrix

type var_dec = {
	var_type : dataType;
	var_name : string;
}

type bin_op = Add | Sub | Times | Divides | Mode | Mtimes | Convolution 

type bool_op1 = Eq | Neq | Lt | Gt | Leq | Geq
type bool_op2 = Not | And | Or

type una_op = Inverse | Minverse | Transpo | Neg

type expr =
	  Det of expr	(* BAR expr BAR *)
	| Trace of expr	(* TRACE LPAREN expr RPAREN *)
	| SubMat of expr * expr * expr * expr * expr	(* SUBMAT LPAREN expr COMMA expr COMMA expr COMMA expr COMMA expr RPAREN *)
	| AreaSum of expr * expr * expr * expr * expr					(* SUM LPAREN expr COMMA expr COMMA expr RPAREN *)
	| Binary_op of expr * bin_op * expr
	| Unary_op of expr * una_op
	| Id of string
	| Float_lit of float
	| Int_lit of int
	| Part_of_mat of expr * expr * expr
	| Boolean of string
	| Im_init of expr list list
	| Float_init of expr list list
	| Call of string * expr list
	| VarAssign of string * expr			(* IDENTIFIER ASSIGN expr SEMICOLON *)
	| ElemAssign of string * expr * expr * expr	(* IDENTIFIER LBRACKET expr RBRACKET ASSIGN expr SEMICOLON *)
	
type b_expr =
	  Bool_expr1 of expr * bool_op1 * expr
	| Bool_expr2 of b_expr * bool_op2 * b_expr

type stmt =
	  Block of stmt list					(* LBRACE Statement_list RBRACE *)
	| Expr of expr							(* expr SEMICOLON *)
	| Ifelse of b_expr * stmt * stmt			(* IF LPAREN expr RPAREN Statement ELSE Statement ENDIF SEMICOLON *)
	| If of b_expr * stmt						(* IF LPAREN expr RPAREN Statement ENDIF SEMICOLON *)
	| For of expr * b_expr * expr * stmt		(* FOR LPAREN expr SEMICOLON expr SEMICOLON expr Statement SEMICOLON *)
	| While of b_expr * stmt					(* WHILE LPAREN expr RPAREN Statement SEMICOLON *)
	| Export of string						(* EXPORT LPAREN IDENTIFIER RPAREN SEMICOLON *)
    | Init of string * expr * expr 
	| Import of string * string				(* IDENTIFIER ASSIGN IMPORT LPAREN IDENTIFIER RPAREN SEMICOLON *)
	| Return of expr
	| Empty

type func_dec = {
	func_type : dataType;
	func_name : string;
	formals : var_dec list;
	locals : var_dec list;
	body : stmt list;
}

type program = var_dec list * func_dec list

(* TESTING *)

let string_of_bool_op1 = function
		 Eq -> "=="
        | Neq -> "!="
        | Lt -> "<"
        | Gt -> ">"
        | Leq -> "<="
        | Geq -> ">="

let string_of_bool_op2 = function
		 And -> "&&"
		| Or -> "||"
		| Not -> "!"


let string_of_bin_op = function
          Add -> "+"
        | Sub -> "-"
        | Times -> "*"
        | Divides -> "/"
        | Mode -> "%"
        | Mtimes -> "*."
        | Convolution -> "**"

let string_of_una_op = function
          Inverse -> "\"" 
        | Transpo -> "'"
        | Neg -> "-"
	| Minverse -> "Nothing" 

let rec string_of_expr = function
          Det(e) -> "(|" ^ string_of_expr e ^ "|)"
        | Trace(e) -> "(tr(" ^ string_of_expr e ^ "))"
        | SubMat(e1,e2,e3,e4,e5) -> "(submat(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ "," ^ string_of_expr e3 ^ "," ^ string_of_expr e4 ^ "," ^ string_of_expr e5 ^ "))"
        | AreaSum(e1,e2,e3,e4,e5) -> "(areasum(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ "," ^ string_of_expr e3 ^ string_of_expr e4 ^ "," ^ string_of_expr e5 ^ "))"
        | Binary_op(e1,op,e2) -> "(" ^ string_of_expr e1 ^ ")" ^ string_of_bin_op op  ^ "(" ^ string_of_expr e2 ^ ")"
        | Unary_op(e,op) -> string_of_una_op op ^ "(" ^ string_of_expr e ^ ")" 
	| Id x -> x
        | Float_lit x -> string_of_float x
	| Int_lit x -> string_of_int x
        | Part_of_mat (e1,e2,e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^  "][" ^ string_of_expr e3 ^ "]"  
        | Boolean x -> x  
	| Im_init x-> "Im_init (" ^ String.concat "," (List.map string_of_expr (List.concat x)) ^ ")"
        | Float_init x-> "Float_init (" ^ String.concat "," (List.map string_of_expr (List.concat x))
	| Call (s,e) -> s ^ "(" ^ String.concat "," (List.map string_of_expr e) ^ ")" 
        | VarAssign (s,e) -> s ^ "=" ^ string_of_expr e
        | ElemAssign (s,e2,e3,e4) -> s ^ "[" ^ string_of_expr e2 ^ "][" ^ string_of_expr e3 ^ "] = " ^ string_of_expr e4


let rec string_of_b_expr = function
		Bool_expr1(e1,b,e2) -> string_of_expr e1 ^ " " ^ string_of_bool_op1 b ^ " " ^ string_of_expr e2 
		| Bool_expr2(b1,b,b2) -> string_of_b_expr b1 ^ " " ^ string_of_bool_op2 b ^ " " ^ string_of_b_expr b2 
		
let rec string_of_stmt = function
          Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
        | Expr(e) -> string_of_expr e ^ ";\n"
        | Ifelse(e1,s1,s2) -> "if (" ^ string_of_b_expr e1 ^ ")\n" ^ "\t" ^ string_of_stmt s1 ^ "\n"      ^ "else \t" ^ string_of_stmt s2 ^ "\n"
        | If (e,s) -> "if (" ^ string_of_b_expr e ^ ")\n\t" ^ string_of_stmt s ^ "\n"
	| For (e1,e2,e3,s) -> "for (" ^ string_of_expr e1 ^ " ; " ^ string_of_b_expr e2 ^ " ; " ^ string_of_expr e3 ^ ")\n\t" ^ string_of_stmt s ^ "\n"
        | While (e,s) -> "while (" ^ string_of_b_expr e ^ ")\n\t" ^ string_of_stmt s
        | Export (s) -> s ^ "\n"
        | Init (s,e1,e2) -> "init (" ^ s ^ " , " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ " )\n"
	| Import (s1,s2) -> "Import " ^ s1 ^ s2
	| Return e -> "return " ^ string_of_expr e
	| Empty -> ""

let string_of_dataType = function
    Int -> "int"
  | Float -> "double"
  | IntRowVec -> "intRowVec"
  | FloatRowVec -> "floatRowVec"
  | IntColVec -> "intColVec"
  | FloatColVec -> "floatColVec"
  | IntMatrix -> "int[][]"
  | FloatMatrix -> "double[][]"

let string_of_matrixType_Init = function
	IntMatrix -> "int"
  | FloatMatrix -> "double"
  | _ -> ""


let string_of_var_dec vdecl = 
	string_of_dataType vdecl.var_type ^ " " ^ vdecl.var_name 	 

let string_of_func_dec fdecl = 
	string_of_dataType fdecl.func_type ^ " " ^  fdecl.func_name ^ 
	 "(" ^ String.concat ", " (List.map string_of_var_dec fdecl.formals) ^ ")\n{\n" ^ 
	String.concat "" (List.map string_of_var_dec fdecl.locals)  ^    
	String.concat "" (List.map string_of_stmt fdecl.body) ^  
	"}\n"	


let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_var_dec vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_func_dec funcs)

