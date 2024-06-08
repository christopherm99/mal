{
  open Parser
  exception SyntaxError of string
}

rule token = parse
  | [' ' '\t' '\r'] { token lexbuf }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+ { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | "fn"       { FN }
  | "let"      { LET }
  | "return"   { RETURN }
  | ['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9']* { IDENT (Lexing.lexeme lexbuf) }
  | '='        { EQ }
  | '+'        { PLUS }
  | '*'        { TIMES }
  | ','        { COMMA }
  | ';'        { SEMICOLON }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { EOF }

