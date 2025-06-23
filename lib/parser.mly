%{
open Ast
%}

%token <string> ID
%token <int> NUMBER
%token IFX // 用于优先 IF-ELSE 移入而非规约
%token INT VOID IF ELSE WHILE BREAK CONTINUE RETURN
%token PLUS MINUS TIMES DIV MOD
%token EQ NEQ LE GE LT GT
%token LAND LOR NOT
%token ASSIGN
%token SEMI COMMA
%token LPAREN RPAREN
%token LBRACE RBRACE
%token EOF

%nonassoc IFX
%nonassoc ELSE
%left LOR
%left LAND
%nonassoc EQ NEQ
%nonassoc LT GT LE GE
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT UMINUS

%start comp_unit
%type <Ast.comp_unit> comp_unit

%%

comp_unit:
  | func_def_list EOF { $1 }

func_def_list:
  | func_def                    { [$1] }
  | func_def_list func_def      { $1 @ [$2] }

func_def:
  | type_spec ID LPAREN param_list_opt RPAREN block
      {
        {
          ret_type = $1;
          func_name = $2;
          params = $4;
          body = $6;
        }
      }

type_spec:
  | INT   { TInt }
  | VOID  { TVoid }

param_list_opt:
  | /* empty */       { [] }
  | param_list        { $1 }

param_list:
  | INT ID                      { [$2] }
  | param_list COMMA INT ID     { $1 @ [$4] }

block:
  | LBRACE stmt_list RBRACE     { $2 }

stmt_list:
  | /* empty */      { [] }
  | stmt_list stmt   { $1 @ [$2] }

stmt:
  | block                    { Block $1 }
  | expr_stmt                { ExprStmt $1 }
  | decl_stmt                { $1 }
  | ID ASSIGN expr SEMI      { Assign ($1, $3) }
  | IF LPAREN expr RPAREN stmt %prec IFX
      { If ($3, $5, None) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
      { If ($3, $5, Some $7) }
  | WHILE LPAREN expr RPAREN stmt
      { While ($3, $5) }
  | BREAK SEMI               { Break }
  | CONTINUE SEMI            { Continue }
  | RETURN expr SEMI         { Return (Some $2) }
  | RETURN SEMI              { Return None }

decl_stmt:
  | INT ID SEMI               { Decl ($2, None) }
  | INT ID ASSIGN expr SEMI   { Decl ($2, Some $4) }

expr_stmt:
  | expr SEMI                 { $1 }

expr:
  | expr LOR land_expr        { LOr ($1, $3) }
  | land_expr                 { $1 }

land_expr:
  | land_expr LAND eq_expr    { LAnd ($1, $3) }
  | eq_expr                   { $1 }

eq_expr:
  | rel_expr EQ rel_expr      { RelBinary ($1, Eq, $3) }
  | rel_expr NEQ rel_expr     { RelBinary ($1, Ne, $3) }
  | rel_expr                  { $1 }

rel_expr:
  | add_expr LT add_expr      { RelBinary ($1, Lt, $3) }
  | add_expr GT add_expr      { RelBinary ($1, Gt, $3) }
  | add_expr LE add_expr      { RelBinary ($1, Le, $3) }
  | add_expr GE add_expr      { RelBinary ($1, Ge, $3) }
  | add_expr                  { $1 }

add_expr:
  | add_expr PLUS mul_expr    { Add ($1, $3) }
  | add_expr MINUS mul_expr   { Sub ($1, $3) }
  | mul_expr                  { $1 }

mul_expr:
  | mul_expr TIMES unary_expr { Mul ($1, $3) }
  | mul_expr DIV unary_expr   { Div ($1, $3) }
  | mul_expr MOD unary_expr   { Mod ($1, $3) }
  | unary_expr                { $1 }

unary_expr:
  | NOT unary_expr            { UnaryOp (Not, $2) }
  | MINUS unary_expr          { UnaryOp (Neg, $2) }
  | PLUS unary_expr           { UnaryOp (Pos, $2) }
  | primary                   { $1 }

primary:
  | ID                        { Primary (ID $1) }
  | NUMBER                    { Primary (Number $1) }
  | ID LPAREN args_opt RPAREN { Primary (Call ($1, $3)) }
  | LPAREN expr RPAREN        { $2 }

args_opt:
  | /* empty */     { [] }
  | args            { $1 }

args:
  | expr                      { [$1] }
  | args COMMA expr           { $1 @ [$3] }