let rec parse = function
  | Seq.Nil -> []
  | Seq.Cons (Token.Semicolon, seq) -> parse (seq ())
  | seq -> parse_statement seq
and parse_statement = function
  | Seq.Cons (Token.Let, seq) -> parse_let (seq ())
and parse_let = function
  | Seq.Cons (Token.Ident typ, seq) ->
      match seq () with
      | Seq.Cons (Token.Eq, seq) ->
          Ast.Let (Ast.Type typ, parse_expr (seq ()))::parse (seq ())
and parse_expr = function
