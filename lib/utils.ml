exception ParseError of Lexing.position

let parse' f s =
  let lexbuf = Lexing.from_string s in
  Lexing.set_filename lexbuf "<stdin>";
  try f Lexer.token lexbuf
  with Parser.Error -> raise (ParseError lexbuf.lex_curr_p)

let parse_program s = parse' Parser.toplevel s
