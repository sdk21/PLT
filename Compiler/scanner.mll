{ open Parser }

let whitespace = [' ' '\t' '\r' '\n'] 
let integers = ['0'-'9']+
let floats = ['0'-'9']+ '.' ['0'-'9']*
let complex = floats '+' floats 'i'
let strings = ['a'-'z' 'A'-'Z']+

rule token = parse
  whitespace { token lexbuf }
| '#'        { comment lexbuf }
| ','        { COMMA }  (* Separate row elements *)
| ';'        { SEMI }   (* Separate column elements *)
| '('        { LPAREN } (* Surround expression *)
| ')'        { RPAREN } 
| '['        { LBRACK } (* Surround vectors/matricies *)
| ']'        { RBRACK }
| '{'        { LBRACE } (* Surround blocks *)
| '}'        { RBRACH }  
| '<'        { LCARR }  (* Open bra- *)
| '>'        { RCARR }  (* Close -ket *)
| '='        { ASSIGN } (* Assignment *)
| '+'        { PLUS }   (* Addition *)
| '-'        { MINUS }  (* Subtraction *)
| '*'        { MULT }   (* Multiplication *)
| '/'        { DIV }    (* Division *)
| '^'        { EXPN }   (* Exponentiation *)
| '|'        { BAR }    (* Close bra- and Open -ket *)
| "eq"       { EQ }     (* Equal to (structural) *)
| "neq"      { NEQ }    (* Not equal to (structural) *) 
| "gt"       { GT }     (* Greater than *)
| "lt"       { LT }     (* Less than *)
| "geq"      { GEQ }    (* Greater than or equal to *)
| "leq"      { LEQ }    (* Less than or equal to *)
| "i"        { I }      (* Indicates imaginary number *)
| "e"        { E }      (* Euler's number *)
| "pi"       { PI }     (* pi *)
| "unit"     { UNIT }   (* Is unit vector? *)
| "im"       { IM }     (* Is imaginary number? *)
| "re"       { RE }     (* Is real number *)
| "norm"     { NORM }   (* Get norm *)
| "trans"    { TRANS }  (* Get transpose *)
| "det"      { DET }    (* Get determinant *) 
| "adj"      { ADJ }    (* Get adjoint *)
| "conj"     { CONJ }   (* Get complex conjugate *)
| "sin"      { SIN }    (* Sine *)
| "cos"      { COS }    (* Cosine *)
| "tan"      { TAN }    (* Tangent *)
| "def"      { DEF }    (* Define function *)
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
| "true"     { TRUE }   (* Boolean true *)
| "false"    { FALSE }  (* Boolean false *)
| "not"      { NOT }    (* Boolean not *)
| "and"      { AND }    (* Boolean and *)
| "or"       { OR }     (* Boolean or *)
| "xor"      { XOR }    (* Boolean xor *)
| "int"      { INT }    (* Integer type *)
| "float"    { FLOAT }  (* Float type *)
| "com"      { COM }    (* Complex type *)
| "mat"      { MAT }    (* Matrix type *)
| eof        { EOF }    (* End of File *)
| integers as lxm  { INT_LIT(int_of_string lxm) }
| floats   as lxm  { FLOAT_LIT(float_of_string lxm) }
| complex  as lxm  { COM(lxm) }
| strings  as lxm  { ID(lxm) }
| _ as char        { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  ['r' '\n']  { token lexbuf }
| _           { comment lexbuf } 
