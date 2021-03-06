%{ open Ast %}

%token C I
%token INT FLOAT COMP MAT
%token DEF
%token ASSIGN
%token COMMA COLON SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE LCAR RCAR BAR
%token PLUS MINUS TIMES DIV MOD EXPN
%token EQ NEQ LT GT LEQ GEQ
%token NOT AND OR XOR
%token TENS UNIT NORM TRANS DET ADJ CONJ IM RE SIN COS TAN
%token IF ELIF ELSE FOR FROM TO BY WHILE BREAK CONT
%token EOF

%token <string> ID
%token <string> INT_LIT
%token <float> FLOAT_LIT
%token <string> COMP_LIT

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR XOR
%left AND
%right NOT
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD TENS
%right EXPN
%nonassoc RE IM NORM TRANS DET ADJ CONJ UNIT SIN COS TAN

%start program
%type <Ast.program> program

%%

vtype:
  INT     { Int }
  | FLOAT { Float }
  | COMP  { Comp }
  | MAT   { Mat }

vdecl:
  vtype ID SEMI { { typ = $1;
                    name = $2 } } 
vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

formal_params:
    /* nothing */      { [] }
  | formal_params_list { List.rev $1 }

formal_params_list:
  vtype ID                            { [{ typ = $1; 
                                           name = $2; }] }
  | formal_params_list COMMA vtype ID { {  typ = $3;
                                           name = $4; } :: $1 }
actual_params:
    /* nothing */      { [] }
  | actual_params_list { List.rev $1 }

actual_params_list:
    expr                          { [$1] }
  | actual_params_list COMMA expr { $3 :: $1 }

fdecl:
   DEF ID LPAREN formal_params RPAREN COLON vtype ID LBRACE vdecl_list stmt_list RBRACE
     { { func_name = $2;
         formal_params = $4;
         ret_typ = $7;
         ret_name = $8;
         locals = List.rev $10;
         body = List.rev $11; } }

mat_row:
    expr               { [$1] }
  | mat_row COMMA expr { $3 :: $1 }

mat_row_list:
    LPAREN mat_row RPAREN              { [List.rev($2)] }
  | mat_row_list LPAREN mat_row RPAREN { List.rev($3) :: $1 }

inner_comp:
    FLOAT_LIT                  { [$1; 0.] }
  | FLOAT_LIT I                { [0.; $1] }
  | FLOAT_LIT PLUS FLOAT_LIT I { [$1; $3] }

expr:
    ID                             { Id($1) }
  | INT_LIT                        { Lit_int(int_of_string $1) }
  | FLOAT_LIT                      { Lit_float($1) }
  | C LPAREN inner_comp RPAREN     { Lit_comp(List.hd $3, List.hd (List.rev $3)) } 
  | LCAR INT_LIT BAR               { Lit_qub($2, 0) }
  | BAR INT_LIT RCAR               { Lit_qub($2, 1) }
  | LBRACK mat_row_list RBRACK     { Mat(List.rev($2)) }
  | LPAREN expr RPAREN             { $2 }
  | ID ASSIGN expr                 { Assign($1, $3) }
  | ID LPAREN actual_params RPAREN { Call($1, $3) }
  | MINUS expr                     { Unop(Neg, $2)}
  | NOT LPAREN expr RPAREN         { Unop(Not, $3) }
  | RE LPAREN expr RPAREN          { Unop(Re, $3) }
  | IM LPAREN expr RPAREN          { Unop(Im, $3) }
  | NORM LPAREN expr RPAREN        { Unop(Norm, $3) }
  | TRANS LPAREN expr RPAREN       { Unop(Trans, $3) }
  | DET LPAREN expr RPAREN         { Unop(Det, $3) }
  | ADJ LPAREN expr RPAREN         { Unop(Adj, $3) }
  | CONJ LPAREN expr RPAREN        { Unop(Conj, $3) }
  | UNIT LPAREN expr RPAREN        { Unop(Unit, $3) }
  | SIN LPAREN expr RPAREN         { Unop(Sin, $3) }
  | COS LPAREN expr RPAREN         { Unop(Cos, $3) }
  | TAN LPAREN expr RPAREN         { Unop(Tan, $3) }
  | expr PLUS   expr               { Binop($1, Add,  $3) }
  | expr MINUS  expr               { Binop($1, Sub,  $3) }
  | expr TIMES  expr               { Binop($1, Mult, $3) }
  | expr DIV    expr               { Binop($1, Div,  $3) }
  | expr MOD    expr               { Binop($1, Mod,  $3) }
  | expr EXPN   expr               { Binop($1, Expn, $3) }
  | expr TENS   expr               { Binop($1, Tens, $3) }
  | expr EQ     expr               { Binop($1, Eq,   $3) }
  | expr NEQ    expr               { Binop($1, Neq,  $3) }
  | expr LT     expr               { Binop($1, Lt,   $3) }
  | expr GT     expr               { Binop($1, Gt,   $3) }
  | expr LEQ    expr               { Binop($1, Leq,  $3) }
  | expr GEQ    expr               { Binop($1, Geq,  $3) }
  | expr OR     expr               { Binop($1, Or,   $3) }
  | expr AND    expr               { Binop($1, And,  $3) }
  | expr XOR    expr               { Binop($1, Xor,  $3) }

 by:
   /* nothing */ { Noexpr }
 | BY expr       { $2 }

stmt:  
    expr SEMI                                        { Expr($1) }
  | LBRACE stmt_list RBRACE                          { Block(List.rev $2) }
  | FOR LPAREN expr FROM expr TO expr by RPAREN stmt { For($3, $5, $7, $8, $10) }
  | WHILE LPAREN expr RPAREN stmt                    { While($3, $5) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE          { If($3, $5, Ast.Expr(Ast.Noexpr)) }
  | IF LPAREN expr RPAREN stmt ELSE stmt             { If($3, $5, $7) }
  | BREAK SEMI                                       { BreakCont(0) }
  | CONT  SEMI                                       { BreakCont(1) }

stmt_list:
    /* nothing  */ { [] }
  | stmt_list stmt { $2 :: $1 }

 rev_program:
   /* nothing */     { [] }
 | rev_program fdecl { $2 :: $1 }

program:
  rev_program { List.rev $1 }