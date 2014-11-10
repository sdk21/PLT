%{ open Ast %}

%token I
%token <float> E
%token <float> PI
%token INT FLOAT COM QUB MAT
%token DEF
%token RETURN
%token ASSIGN
%token COMMA SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE LCAR RCAR BAR
%token PLUS MINUS TIMES DIV MOD EXPN
%token EQ NEQ LT GT LEQ GEQ
%token TRUE FALSE NOT AND OR XOR
%token TENS UNIT NORM TRANS DET ADJ CONJ
%token IM RE SIN COS TAN
%token IF ELIF ELSE FOR FROM TO BY WHILE BREAK CONT
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

%start program
%type <Ast.program> program

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
  vtype ID SEMICOLON { { v_type = $1;
                         v_name = $2 } } 
vdecl_list:
  /* nothing */  { [] }
  | vdecl_list vdecl { $2 :: $1 }

formal_params:
  /* nothing */ { [] }
  formal_params_list { List.rev $1 }

formal_params_list:
  vtype ID                          { { var_type = $1; 
                                        var_name = $2; } }
  formal_params_list COMMA vtype ID { {  var_type = $3;
                                         var_name = $4; } :: $1 }

fdecl:
   DEF ID EQ ID LPAREN RPAREN
   ID LPAREN EQ formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = List.rev $6;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   INT ID SEMI { $2 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
