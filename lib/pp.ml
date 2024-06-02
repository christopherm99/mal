open Format
open Syntax

let pp_comma ppf () = fprintf ppf ",@ "
let pp_ident ppf s = fprintf ppf "%s" s

let rec pp_expr ppf = function
  | Variable name -> pp_ident ppf name
  | Number n -> fprintf ppf "%d" n
  | Plus (e1, e2) -> fprintf ppf "%a@ +@ %a" pp_expr e1 pp_expr e2

let rec pp_stmt ppf = function
  | Function (Prototype (name, args), body) ->
      fprintf ppf "@[<hov>fun %s(%a) {@;@[<hov 2>%a@]@;}"
        name (pp_print_list ~pp_sep:pp_comma pp_ident) args pp_stmt body 
  | Let (name, e) ->
      fprintf ppf "let %s = %a;" name pp_expr e
  | Return e ->
      fprintf ppf "return %a;" pp_expr e

