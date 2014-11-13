{ open Lamepar }

rule token =
  parse [' ' '\t' '\r' '\n'] { token lexbuf }
      | eof                  { EOF }
      | '+'                  { PLUS }
      | '-'                  { MINUS }
      | '*'                  { TIMES }
      | '/'                  { DIVIDE }
      | '^'                  { POWER }
      | ','                  { COMMA }
      | ';'                  { SEMICOLON }
      | '{'                  { LBRACE }
      | '}'                  { RBRACE }
      | '('                  { LPAREN }
      | ')'                  { RPAREN }
      | '['                  { LBRACK }
      | ']'                  { RBRACK }
      | "=="                 { EQ }
      | "!="                 { NE }
      | '<'                  { LT }
      | "<="                 { LE }
      | '>'                  { GT }
      | ">="                 { GE }
      | '!'                  { NOT }
      | "||"                 { OR }
      | "&&"                 { AND }
      | '\''                 { TRANS }
      | '='                  { ASSIGN }
      | "dim"                { DIM }
      | "if"                 { IF }
      | "else"               { ELSE }
      | "while"              { WHILE }
      | "print"              { PRINT }
      | "true"               { TRUE }
      | "false"              { FALSE }
      | "scalar"             { SCALAR }
      | "string"             { STRING }
      | "matrix"             { MATRIX }
      | "boolean"            { BOOLEAN }
      | "size_cols"          { SIZE_COLS }
      | "size_rows"          { SIZE_ROWS }
      | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as id { IDENT(id) }
      | ['0'-'9']+ ('.' ['0'-'9']+)? (['e' 'E'] ['-' '+']? ['0'-'9']+)? as num { NUM(float_of_string num) }
      | '"' ([^ '"' '\n' '\r' '\\'] | "\\\\" | "\\\"" | "\\n" | "\\t")* '"' as str { STRLIT(str) }
