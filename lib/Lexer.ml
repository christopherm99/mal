let rec lex = function
  | Seq.Nil -> Seq.Nil
  | Seq.Cons ((' ' | '\n' | '\r' | '\t'), seq) -> lex (seq ())
  | Seq.Cons (';', seq) -> Seq.Cons (Token.Semicolon, fun () -> lex (seq ()))
  | Seq.Cons (('A' .. 'Z' | 'a' .. 'z' as c), seq) ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_ident buffer (seq ())
  | Seq.Cons (('0' .. '9' as c), seq) ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_number buffer (seq ())
  | Seq.Cons ('#', seq) ->
      lex_comment (seq ())
  | seq ->
      lex_keyword seq
and lex_ident buffer = function
  | Seq.Cons (('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c), seq) ->
      Buffer.add_char buffer c;
      lex_ident buffer (seq ())
  | seq ->
      match Buffer.contents buffer with
      | "fn" -> Seq.Cons (Token.Fn, fun () -> lex seq)
      | "let" -> Seq.Cons (Token.Let, fun () -> lex seq)
      | id -> Seq.Cons (Token.Ident id, fun () -> lex seq)
and lex_number buffer = function
  | Seq.Cons (('0' .. '9' as c), seq) ->
      Buffer.add_char buffer c;
      lex_number buffer (seq ())
  | seq ->
      Seq.Cons (Number (float_of_string (Buffer.contents buffer)), fun () -> lex seq)
and lex_comment = function
  | Seq.Cons ('\n', seq) -> lex (seq ())
  | Seq.Cons (_, seq) -> lex_comment (seq ())
  | Seq.Nil -> Seq.Nil
and lex_keyword = function
  | Seq.Cons (';', seq) -> Seq.Cons (Token.Semicolon, fun () -> lex (seq ()))
  | Seq.Cons ('=', seq) -> Seq.Cons (Token.Eq, fun () -> lex (seq ()))

