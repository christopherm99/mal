%{
  open Syntax
%}

/* Tokens */
%token <string> IDENT
%token <int> NUMBER
%token FN
%token LET
%token RETURN
%token EQ
%token COMMA
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token EOF

%start toplevel
%type <Syntax.statement list> toplevel

%%

/* Grammar */

toplevel:
  | EOF                { [] }
  | statement toplevel { $1 :: $2 }

statement:
  | FN IDENT LPAREN args RPAREN LBRACE statement RBRACE
    { Function (Prototype ($2, $4), $7) }
  | LET IDENT EQ expr SEMICOLON
    { Let ($2, $4) }
  | RETURN expr SEMICOLON
    { Return $2 }
;

expr:
  | IDENT  { Variable $1 }
  | NUMBER { Number $1 }
;

args:
  separated_list(COMMA, IDENT) { $1 }

