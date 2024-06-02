{
  open Parser
}

rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+ { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | "fn"       { FN }
  | "let"      { LET }
  | "return"   { RETURN }
  | ['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9']* { IDENT (Lexing.lexeme lexbuf) }
  | '='        { EQ }
  | ','        { COMMA }
  | ';'        { SEMICOLON }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | eof        { EOF }
