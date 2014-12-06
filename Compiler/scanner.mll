(* Scanner
    - Consumes program as symbol stream
    - Produces token stream for parser
*)

{ open Parser }

let whitespace = [' ' '\t' '\r' '\n'] 
let name = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let integers = ['0'-'9']+
let floats = ['0'-'9']+ '.' ['0'-'9']*

rule token = parse
  whitespace { token lexbuf }
| '#'        { comment lexbuf }

| "int"      { INT }    (* Integer type *)
| "float"    { FLOAT }  (* Float type *)
| "comp"     { COMP }   (* Complex type *)
| "mati"     { MATI }   (* Matrix type (int) *)
| "matf"     { MATF }   (* Matrix type (float) *)
| "matc"     { MATC }   (* Matrix type (comp) *)
| "qubb"     { QUBB }   (* Qubit type (bra) *)
| "qubk"     { QUBK }   (* Qubit type (ket) *)

| "C"        { C }      (* Start of complex number *)
| "I"        { I }      (*  Imaginary component *)

| "def"      { DEF }    (* Define function *)
| "return"   { RETURN }

| '='        { ASSIGN } (* Assignment *)
| ','        { COMMA }  (* Separate list elements *)
| ':'        { COLON }  (* Separate matrix rows *)
| ';'        { SEMI }   (* Separate matrix columns *)
| '('        { LPAREN } (* Surround expression *)
| ')'        { RPAREN } 
| '['        { LBRACK } (* Surround vectors/matricies *)
| ']'        { RBRACK }
| '{'        { LBRACE } (* Surround blocks *)
| '}'        { RBRACE }  
| '<'        { LCAR }   (* Open bra- *)
| '>'        { RCAR }   (* Close -ket *)
| '|'        { BAR }    (* Close bra- and Open -ket *)

| '+'        { PLUS }   (* Addition *)
| '-'        { MINUS }  (* Subtraction *)
| '*'        { TIMES }  (* Multiplication *)
| '/'        { DIV }    (* Division *)
| '%'        { MOD }    (* Modulus *)
| '^'        { EXPN }   (* Exponentiation *)

| "eq"       { EQ }     (* Equal to (structural) *)
| "neq"      { NEQ }    (* Not equal to (structural) *) 
| "lt"       { LT }     (* Less than *)
| "gt"       { GT }     (* Greater than *)
| "leq"      { LEQ }    (* Less than or equal to *)
| "geq"      { GEQ }    (* Greater than or equal to *)

| "true"     { TRUE }   (* Boolean true *)
| "false"    { FALSE }  (* Boolean false *)
| "not"      { NOT }    (* Boolean not *)
| "and"      { AND }    (* Boolean and *)
| "or"       { OR }     (* Boolean or *)
| "xor"      { XOR }    (* Boolean xor *)

| "norm"     { NORM }   (* Get norm *)
| "trans"    { TRANS }  (* Get transpose *)
| "det"      { DET }    (* Get determinant *) 
| "adj"      { ADJ }    (* Get adjoint *)
| "conj"     { CONJ }   (* Get complex conjugate *)
| "unit"     { UNIT }   (* Is unit matrix? *)
| '@'        { TENS }   (* Tensor product *)

| "im"       { IM }     (* Is imaginary number? *)
| "re"       { RE }     (* Is real number *)
| "sin"      { SIN }    (* Sine *)
| "cos"      { COS }    (* Cosine *)
| "tan"      { TAN }    (* Tangent *)

| "if"       { IF }     (* If statement *)
| "else"	 { ELSE }	(* Else statement *)
| "for"      { FOR }    (* For loop - for(i from x to y by z) *)
| "from"     { FROM }
| "to"       { TO }
| "by"       { BY }
| "while"    { WHILE }  (* While loop *)
| "break"    { BREAK }  (* Break For or While loop *)
| "continue" { CONT }   (* Continue to For or While loop *)

| name     as lxm  { ID(lxm) } 
| integers as lxm  { INT_LIT(lxm) }
| floats   as lxm  { FLOAT_LIT(float_of_string lxm) }

| eof              { EOF }
| _ as char        { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  ['r' '\n']       { token lexbuf }
| _                { comment lexbuf } 
