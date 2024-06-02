type expr =
  | Variable of string
  | Number of int
  | Plus of expr * expr

type proto = Prototype of string * string list

type statement =
  | Function of proto * statement
  | Let of string * expr
  | Return of expr

