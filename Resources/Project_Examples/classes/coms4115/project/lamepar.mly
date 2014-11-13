%{
    (* Carmine Elvezio *)
    open Ast
%}

%token PLUS MINUS TIMES DIVIDE POWER COMMA SEMICOLON
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token ASSIGN EQ NE LT LE GT GE NOT OR AND TRANS
%token IF ELSE WHILE PRINT TRUE FALSE
%token STRING SCALAR MATRIX BOOLEAN
%token SIZE_ROWS SIZE_COLS DIM
%token EOF
%token <string> IDENT
%token <string> STRLIT
%token <float> NUM

%nonassoc NOELSE
%nonassoc ELSE

%nonassoc NOSEMI
%nonassoc SEMICOLON

%nonassoc NOCOMMA
%nonassoc COMMA

%left OR
%left AND
%nonassoc EQ NE
%left LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left POWER
%nonassoc NOT
%nonassoc UMINUS
%nonassoc TRANS
%nonassoc SIZE_ROWS SIZE_COLS

%start prgm
%type <float list> row
%type <row list> matrix
%type <lvalue> lvalue
%type <expr> expr
%type <datatype> datatype
%type <stmt> stmt
%type <stmt list> stmtlist
%type <Ast.stmt list> prgm

%%

row:
  NUM COMMA row         {$1 :: $3}
| NUM %prec NOCOMMA     {[$1]}

matrix:
  row SEMICOLON matrix  {$1 :: $3}
| row %prec NOSEMI      {[$1]}

lvalue:
  IDENT                                     {Ident($1)}
| IDENT LBRACK expr RBRACK                  {VAccess($1, $3)}
| IDENT LBRACK expr COMMA expr RBRACK       {MAccess($1, $3, $5)}

expr:
  NUM                        {NumLit($1)}
| STRLIT                     {StrLit($1)}
| TRUE                       {BoolLit(true)}
| FALSE                      {BoolLit(false)}
| lvalue                     {LValue($1)}
| expr PLUS expr             {Plus($1, $3)}
| expr MINUS expr            {Minus($1, $3)}
| expr TIMES expr            {Times($1, $3)}
| expr DIVIDE expr           {Divide($1, $3)}
| expr POWER expr            {Power($1, $3)}
| MINUS expr %prec UMINUS    {Times(NumLit(-1.0), $2)}
| expr EQ expr               {Eq($1, $3)}
| expr NE expr               {Neq($1, $3)}
| expr LT expr               {Lt($1, $3)}
| expr GT expr               {Gt($1, $3)}
| expr LE expr               {Le($1, $3)}
| expr GE expr               {Ge($1, $3)}
| NOT expr                   {Not($2)}
| expr AND expr              {And($1, $3)}
| expr OR expr               {Or($1, $3)}
| expr TRANS                 {Trans($1)}
| LPAREN expr RPAREN         {$2}
| LBRACE matrix RBRACE       {MatLit($2)}
| SIZE_ROWS expr             {SizeRows($2)}
| SIZE_COLS expr             {SizeCols($2)}

datatype:
  SCALAR                {Scalar}
| STRING                {String}
| MATRIX                {Matrix}
| BOOLEAN               {Boolean}

stmt:
  lvalue ASSIGN expr SEMICOLON                      {Assign($1, $3)}
| IF LPAREN expr RPAREN stmt %prec NOELSE           {If($3, $5) }
| IF LPAREN expr RPAREN stmt ELSE stmt              {IfElse($3, $5, $7)}
| WHILE LPAREN expr RPAREN stmt                     {While($3, $5)}
| PRINT expr SEMICOLON                              {Print($2)}
| DIM IDENT LBRACK expr COMMA expr RBRACK SEMICOLON {Dim($2, $4, $6)}
| datatype IDENT SEMICOLON                          {Decl($1, $2)}
| datatype IDENT ASSIGN expr SEMICOLON              {DeclInit($1, $2, $4)}
| LBRACE stmtlist RBRACE                            {StmtList($2)}

stmtlist:
  stmt stmtlist                             {$1 :: $2}
| stmt                                      {[$1]}

prgm:
  stmt EOF              {[$1]}
| stmt prgm             {$1 :: $2}
