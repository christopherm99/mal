%{
  open Syntax
%}

/* Tokens */
%token <string> IDENT
%token <int> NUMBER
%token FN
%token LET
%token IF
%token ELSE
%token RETURN
%token EQ
%token PLUS
%token TIMES
%token MINUS
%token COMMA
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token EOF

%left  PLUS MINUS
%left  TIMES

%start toplevel
%type <Syntax.statement list> toplevel

%%

/* Grammar */

toplevel:
  | EOF                { [] }
  | statement toplevel { $1 :: $2 }

statement:
  | FN IDENT LPAREN args RPAREN LBRACE statement+ RBRACE
    { Function (Prototype ($2, $4), $7) }
  | LET IDENT EQ expr SEMICOLON
    { Let ($2, $4) }
  | IF LPAREN expr RPAREN LBRACE statement+ RBRACE
    { If ($3, $6, None) }
  | IF LPAREN expr RPAREN LBRACE statement+ RBRACE ELSE LBRACE statement+ RBRACE
    { If ($3, $6, Some $10) }
  | RETURN expr SEMICOLON
    { Return $2 }
;

expr:
  | IDENT                                           { Variable $1 }
  | NUMBER                                          { Number $1 }
  | expr PLUS expr                                  { Binary (Plus, $1, $3) }
  | expr TIMES expr                                 { Binary (Times, $1, $3) }
  | expr MINUS expr                                 { Binary (Minus, $1, $3) }
  | IDENT LPAREN separated_list(COMMA, expr) RPAREN { Call ($1, $3) }
;

args:
  separated_list(COMMA, IDENT) { $1 }

