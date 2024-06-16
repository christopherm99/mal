type op = Plus | Times

type expr =
  | Variable of string
  | Number of int
  | Binary of op * expr * expr
  | Call of string * expr list

type proto = Prototype of string * string list

type statement =
  | Function of proto * statement list
  | Let of string * expr
  | Return of expr
