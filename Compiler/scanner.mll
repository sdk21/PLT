{ open Parser }

let name = ['a'-'z' 'A'-'Z']+ 
let whitespace = [' ' '\t' '\r' '\n'] 
let integers = ['0'-'9']+
let floats = ['0'-'9']+ '.' ['0'-'9']*
let complex = floats '+' floats 'i'

rule token = parse
(* Whitespace *)
  whitespace { token lexbuf }

(* Comments *)
| '#'        { comment lexbuf }

(* Function Declaration *)
| "def"      { DEF }    (* Define function *)

(* Built-in Constants *)
| "i"        { I }      (* Indicates imaginary number *)
| "e"        { E }      (* Euler's number *)
| "pi"       { PI }     (* pi *)

(* Built-in Types *)
| "int"      { INT }    (* Integer type *)
| "float"    { FLOAT }  (* Float type *)
| "com"     { COM }   (* Complex type *)
| "qub"      { QUB }    (* Qubit type *)
| "mat"      { MAT }    (* Matrix type *)
|
(* Punctuation *)
| ','        { COMMA }  (* Separate row elements *)
| ';'        { SEMI }   (* Separate column elements *)
| '('        { LPAREN } (* Surround expression *)
| ')'        { RPAREN } 
| '['        { LBRACK } (* Surround vectors/matricies *)
| ']'        { RBRACK }
| '{'        { LBRACE } (* Surround blocks *)
| '}'        { RBRACE }  
| '<'        { LCAR }   (* Open bra- *)
| '>'        { RCAR }   (* Close -ket *)
| '|'        { BAR }    (* Close bra- and Open -ket *)

(* Algebraic Operators *)
| '='        { ASSIGN } (* Assignment *)
| '+'        { PLUS }   (* Addition *)
| '-'        { MINUS }  (* Subtraction *)
| '*'        { TIMES }   (* Multiplication *)
| '/'        { DIV }    (* Division *)
| '%'        { MOD }    (* Modulus *)
| '^'        { EXPN }   (* Exponentiation *)

(* Relational Operators *)
| "eq"       { EQ }     (* Equal to (structural) *)
| "neq"      { NEQ }    (* Not equal to (structural) *) 
| "gt"       { GT }     (* Greater than *)
| "lt"       { LT }     (* Less than *)
| "geq"      { GEQ }    (* Greater than or equal to *)
| "leq"      { LEQ }    (* Less than or equal to *)

(* Boolean Values & Operators *)
| "true"     { TRUE }   (* Boolean true *)
| "false"    { FALSE }  (* Boolean false *)
| "not"      { NOT }    (* Boolean not *)
| "and"      { AND }    (* Boolean and *)
| "or"       { OR }     (* Boolean or *)
| "xor"      { XOR }    (* Boolean xor *)

(* Matrix Operators *)
| '@'        { TENS }   (* Tensor product *)
| "unit"     { UNIT }   (* Is unit matrix? *)
| "norm"     { NORM }   (* Get norm *)
| "trans"    { TRANS }  (* Get transpose *)
| "det"      { DET }    (* Get determinant *) 
| "adj"      { ADJ }    (* Get adjoint *)
| "conj"     { CONJ }   (* Get complex conjugate *)

(* Built-in Functions  *)
| "im"       { IM }     (* Is imaginary number? *)
| "re"       { RE }     (* Is real number *)
| "sin"      { SIN }    (* Sine *)
| "cos"      { COS }    (* Cosine *)
| "tan"      { TAN }    (* Tangent *)

(* Control Flow *)
| "if"       { IF }     (* If statement *)
| "elif"     { ELIF }   (* Else if statement *)
| "else"     { ELSE }   (* Else statement *)
| "for"      { FOR }    (* For loop (For i from x to y by z *)
| "from"     { FROM }
| "to"       { TO }
| "by"       { BY }
| "while"    { WHILE }  (* While loop *)
| "break"    { BREAK }  (* Break For or While loop *)
| "continue" { CONT }   (* Continue to For or While loop *)

(* Identifiers *)
| name as id { ID(id) } 

(* Numbers *)
| integers as lxm  { INT_LIT(int_of_string lxm) }
| floats   as lxm  { FLOAT_LIT(float_of_string lxm) }
| complex  as lxm  { COM(lxm) }

(* End of File *)
 eof        { EOF }    (* End of File *)

(* Illegal Input *)
| _ as char        { raise (Failure("illegal character " ^ Char.escaped char)) }

(* Comment Parsing *)
and comment = parse
  ['r' '\n']  { token lexbuf }
| _           { comment lexbuf } 
