%{ open Ast
let e = 2.718281828
let pi = 3.141592653 %}

%token I
%token E
%token PI
%token INT FLOAT COM QUB MAT
%token DEF
%token RETURN
%token ASSIGN
%token COMMA COLON SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE LCAR RCAR BAR
%token PLUS MINUS TIMES DIV MOD EXPN
%token EQ NEQ LT GT LEQ GEQ
%token TRUE FALSE NOT AND OR XOR
%token TENS UNIT NORM TRANS DET ADJ CONJ
%token IM RE SIN COS TAN
%token IF ELIF ELSE FOR FROM TO BY WHILE BREAK CONT
%token EOF
%token <string> ID
%token <int> INT_LIT
%token <float> FLOAT_LIT

%right ASSIGN
%left NOT AND OR XOR
%left EQ NEQ UNIT
%left LT GT LEQ GEQ
%left SIN COS TAN
%left IM RE
%left PLUS MINUS TENS
%left MULT DIV MOD NORM TRANS DET ADJ CONJ
%right EXPN

%start expr
/*%type <Ast.program> programi*/
%type <Ast.expr> expr

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

vtype:
  INT     { Int }
  | FLOAT { Float }
  | COM   { Com }
  | QUB   { Qub }
  | MAT   { Mat }

vdecl:
  vtype ID SEMI { { var_type = $1;
                    var_name = $2 } } 
vdecl_list:
  /* nothing */      { [] }
  | vdecl_list vdecl { $2 :: $1 }

formal_params:
  /* nothing */      { [] }
  | formal_params_list { List.rev $1 }

formal_params_list:
  vtype ID                            { [{ var_type = $1; 
                                          var_name = $2; }] }
  | formal_params_list COMMA vtype ID { {  var_type = $3;
                                           var_name = $4; } :: $1 }

actual_params:
  /* nothing */        { [] }
  | actual_params_list { List.rev $1 }

actual_params_list:
    expr                          { [$1] }
  | actual_params_list COMMA expr { $3 :: $1 }

fdecl:
   DEF vtype ID EQ ID LPAREN formal_params RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { ret_type = $2;
         ret_name = $3;
         func_name = $5;
	 formal_params = $7;
	 locals = List.rev $10;
	 body = List.rev $11; } }

mat_row_list:
  LPAREN mat_row RPAREN { [List.rev $2] }
  | mat_row_list LPAREN mat_row RPAREN { $3 :: $1 }

mat_row:
   expr { [$1] }
  | mat_row COMMA expr { $3 :: $1 }

expr:
  ID                               { Id($1) }
  | INT_LIT                        { Lit_int($1) }
  | FLOAT_LIT                      { Lit_float($1) }
  | FLOAT_LIT PLUS FLOAT_LIT I     { Com($1, $3) }
  | LCAR expr BAR                  { Qub_bra($2) }
  | BAR expr RCAR                  { Qub_ket($2) }
  | E                              { Lit_float(e) }
  | PI                             { Lit_float(pi) }
  | LBRACK mat_row_list RBRACK     { Mat($2) }
  | LPAREN expr RPAREN             { $2 }
  | ID ASSIGN expr                 { Assign($1, $3) }
  | ID LPAREN actual_params RPAREN { Call($1, $3) }
  | NOT expr                       { Unop(Not, $2) }
  | RE expr                        { Unop(Re, $2) }
  | IM expr                        { Unop(Im, $2) }
  | NORM expr                      { Unop(Norm, $2) }
  | UNIT expr                      { Unop(Unit, $2) }
  | TRANS expr                     { Unop(Trans, $2) }
  | DET expr                       { Unop(Det, $2) }
  | ADJ expr                       { Unop(Adj, $2) }
  | CONJ expr                      { Unop(Con, $2) }
  | SIN expr                       { Unop(Sin, $2) }
  | COS expr                       { Unop(Cos, $2) }
  | TAN expr                       { Unop(Tan, $2) }
  | EXPN expr                      { Unop(Exp, $2) }
  | expr PLUS   expr               { Binop($1, Add,  $3) }
  | expr MINUS  expr               { Binop($1, Sub,  $3) }
  | expr TIMES  expr               { Binop($1, Mult, $3) }
  | expr DIV    expr               { Binop($1, Div,  $3) }
  | expr MOD    expr               { Binop($1, Mod,  $3) }
  | expr EQ     expr               { Binop($1, Eq,   $3) }
  | expr NEQ    expr               { Binop($1, Neq,  $3) }
  | expr LT     expr               { Binop($1, Lt,   $3) }
  | expr GT     expr               { Binop($1, Gt,   $3) }
  | expr LEQ    expr               { Binop($1, Leq,  $3) }
  | expr GEQ    expr               { Binop($1, Geq,  $3) }
  | expr OR     expr               { Binop($1, Or,   $3) }
  | expr AND    expr               { Binop($1, And,  $3) }
  | expr XOR    expr               { Binop($1, Xor,  $3) }
  | expr TENS   expr               { Binop($1, Tens,  $3) }

 by:
  /* nothing */ { Noexpr }
 | BY expr      { $2 }

stmt:  
  expr SEMI                            { Expr($1) }
  | LBRACE stmt_list RBRACE            { Block(List.rev $2) }
  | FOR expr FROM expr TO expr by stmt { For($2, $4, $6, $7, $8) }
  | WHILE LPAREN expr RPAREN stmt      { While($3, $5) }
  | IF LPAREN expr RPAREN stmt         { If($3, $5) }

stmt_list:
  /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }
