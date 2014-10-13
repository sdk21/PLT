{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '#'  { COMMENT }
| ','  { COMMA }  
| '_'  { UNDER } 
| ';'  { SEMI }
| ':'  { COLON }
| '('  { LPAREN }
| ')'  { RPAREN }
| '['  { LBRACK }
| ']'  { RBRACK }
| '{'  { LBRACE }
| '}'  { RBRACH }
| '<'  { LCARR }
| '>'  { RCARR }
| '='  { EQ }
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { MULT }
| '/'  { DIV }
| '|'  { BAR }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']+ as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
