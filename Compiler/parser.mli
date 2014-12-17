type token =
  | C
  | I
  | INT
  | FLOAT
  | COMP
  | MAT
  | DEF
  | RETURN
  | ASSIGN
  | COMMA
  | COLON
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | LCAR
  | RCAR
  | BAR
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EXPN
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | TRUE
  | FALSE
  | NOT
  | AND
  | OR
  | XOR
  | TENS
  | UNIT
  | NORM
  | TRANS
  | DET
  | ADJ
  | CONJ
  | IM
  | RE
  | SIN
  | COS
  | TAN
  | IF
  | ELIF
  | ELSE
  | FOR
  | FROM
  | TO
  | BY
  | WHILE
  | BREAK
  | CONT
  | EOF
  | ID of (string)
  | INT_LIT of (string)
  | FLOAT_LIT of (float)
  | COMP_LIT of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
