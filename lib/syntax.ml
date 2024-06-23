type op = Plus | Times | Minus

type expr =
  | Variable of string
  | Number of int
  | Binary of op * expr * expr
  | Call of string * expr list

type proto = Prototype of string * string list

type statement =
  | Function of proto * statement list
  | Let of string * expr
  | If of expr * statement list * statement list option
  | Return of expr
