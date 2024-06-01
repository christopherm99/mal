type expr =
  | Number of float
  | Variable of string
  | Binary of char
  | Call of string * expr array

(* Function Prototype: function name and argument names *)
type proto = Prototype of string * string array

(* Function definition *)
type func = Function of proto * expr

type typ = Type of string

type var = Let of typ * expr

