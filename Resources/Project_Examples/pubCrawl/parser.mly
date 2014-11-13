%{ open Ast

  (* convert string into char list *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA ASSIGN COLON ARROW CONCAT ACCESS
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token RETURN IF ELIF ELSE FOR WHILE DISTRIBUTE
%token <string> ID
%token <float> NUM_LIT
%token <bool> BOOLEAN_LIT
%token <string> STRING_LIT
%token <char> CHAR_LIT
%token EOF

%nonassoc ASSIGN COLON
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS CONCAT
%left TIMES DIVIDE MOD
%right NOT
%left ACCESS
%left LBRACK RBRACK
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%

program:
  stmt_list { List.rev $1 }

/***************
    STATEMENTS 
***************/

stmt_list:
   /* nothing */  { [] }
 | stmt_list stmt { $2 :: $1 }

body:
   LBRACE stmt_list RBRACE { List.rev $2 }

stmt:
    assignment SEMI { $1 }
  | func_call SEMI  { FuncCallStmt(fst $1, snd $1) }
  | FOR LPAREN assign_opt SEMI expr_opt SEMI assign_opt RPAREN body 
      { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN body
      { While($3, $5) }
  | IF LPAREN expr RPAREN body elifs else_opt
      { If({condition=$3;body=$5} :: $6, $7) }
  | RETURN expr SEMI { Return($2) }

assign_opt:
    /* nothing */ { None }
  | assignment    { match $1 with Assign(e1, e2) -> Some(e1, e2) | _ -> None }

assignment:
    ID ASSIGN expr     { Assign(Id($1), $3) }
  | access ASSIGN expr { Assign($1, $3) }

/***************
    EXPRESSIONS 
***************/

expr_opt:
    /* nothing */ { None }
  | expr          { Some($1) }

expr:
    NUM_LIT            { NumLit($1) }
  | BOOLEAN_LIT        { BoolLit($1) }
  | CHAR_LIT           { CharLit($1) }
  | STRING_LIT         { ListCreate(List.map (fun x -> CharLit(x)) (explode $1)) }
  | ID                 { Id($1) }
  | func_create        { $1 }
  | func_call          { FuncCallExpr(fst $1, snd $1) }
  | access             { $1 }
  | list_create        { $1 }
  | obj_create         { $1 }
  | LPAREN expr RPAREN { $2 }
  | expr CONCAT expr   { Binop($1, Concat,  $3) }
  | expr PLUS   expr   { Binop($1, Add,     $3) }
  | expr MINUS  expr   { Binop($1, Sub,     $3) }
  | expr TIMES  expr   { Binop($1, Mult,    $3) }
  | expr DIVIDE expr   { Binop($1, Div,     $3) }
  | expr MOD    expr   { Binop($1, Mod,     $3) }
  | expr EQ     expr   { Binop($1, Equal,   $3) }
  | expr NEQ    expr   { Binop($1, Neq,     $3) }
  | expr LT     expr   { Binop($1, Less,    $3) }
  | expr LEQ    expr   { Binop($1, Leq,     $3) }
  | expr GT     expr   { Binop($1, Greater, $3) }
  | expr GEQ    expr   { Binop($1, Geq,     $3) }
  | expr AND    expr   { Binop($1, And,     $3) }
  | expr OR     expr   { Binop($1, Or,      $3) }
  | NOT expr           { Not($2) }

elifs:
    /* nothing */                      { [] }
  | ELIF LPAREN expr RPAREN body elifs { {condition=$3;body=$5} :: $6 }

else_opt:
    /* nothing */ { None }
  | ELSE body     { Some($2) }

/***************
    FUNCTIONS 
***************/

/* 
  1. () -> body
  2. (x) -> body 
  3. (x,y,z) -> body 
  4. x -> body
*/
func_create:
    LPAREN RPAREN ARROW body              { FuncCreate([], $4) }
  | LPAREN expr RPAREN ARROW body         { match $2 with
                                              Id(x) -> FuncCreate([x], $5) 
                                            | _ -> failwith "Invalid function creation."
                                          }
  | LPAREN mult_formals RPAREN ARROW body { FuncCreate($2, $5) }
  | ID ARROW body                         { FuncCreate([$1], $3) }

mult_formals:
  formal_list COMMA ID { List.rev ($3 :: $1) }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

func_call:
  expr LPAREN actuals_opt RPAREN { ($1, $3) }
  | DISTRIBUTE LPAREN actuals_opt RPAREN { (Id("distribute"), $3)}

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

/***************
  OBJECTS AND LISTS
***************/

obj_create: 
    LBRACE RBRACE            { ObjCreate([]) }
  | LBRACE properties RBRACE { ObjCreate(List.rev $2) }

properties: 
    ID COLON expr                  { [($1, $3)] }
  | properties COMMA ID COLON expr { ($3, $5) :: $1 }

list_create:
    LBRACK actuals_opt RBRACK                  { ListCreate($2) }
  | expr LBRACK expr_opt COLON expr_opt RBRACK { Sublist($1, $3, $5) }

access:
    obj_access  { $1 }
  | list_access { $1 }

obj_access:
    expr ACCESS ID { ObjAccess($1, $3) }

list_access: 
    expr LBRACK expr RBRACK { ListAccess($1, $3) }
