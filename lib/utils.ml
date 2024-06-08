exception ParseError of Lexing.position
exception LexError of Lexing.position * string

let parse' f s =
  let lexbuf = Lexing.from_string s in
  Lexing.set_filename lexbuf "<stdin>";
  try f Lexer.token lexbuf with
  | Parser.Error -> raise (ParseError lexbuf.lex_curr_p)
  | Lexer.SyntaxError msg -> raise (LexError (lexbuf.lex_curr_p, msg))

let parse_program s = parse' Parser.toplevel s
