type token =
  | Semicolon | Eq
  | Fn | Let
  | Ident of string | Number of float
  | Kwd of char

