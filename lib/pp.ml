open Format
open Syntax

let pp_comma ppf () = fprintf ppf ",@ "
let pp_newl ppf () = fprintf ppf "@;"
let pp_ident ppf s = fprintf ppf "%s" s

let rec pp_expr ppf = function
  | Variable name -> pp_ident ppf name
  | Number n -> fprintf ppf "%d" n
  | Binary (op, e1, e2) -> (
      match op with
      | Plus -> fprintf ppf "%a@ +@ %a" pp_expr e1 pp_expr e2
      | Times -> fprintf ppf "%a@ *@ %a" pp_expr e1 pp_expr e2
      | Minus -> fprintf ppf "%a@ -@ %a" pp_expr e1 pp_expr e2)
  | Call (name, args) ->
      fprintf ppf "%s(%a)" name (pp_print_list ~pp_sep:pp_comma pp_expr) args

and pp_stmt ppf = function
  | Function (Prototype (name, args), body) ->
      fprintf ppf "@[<hov>fn %s(%a) {@;@[<hov 2>%a@]@;}" name
        (pp_print_list ~pp_sep:pp_comma pp_ident)
        args
        (pp_print_list ~pp_sep:pp_newl pp_stmt)
        body
  | Let (name, e) -> fprintf ppf "let %s = %a;" name pp_expr e
  | If (cond, then_, Some else_) ->
      fprintf ppf "if (%a) { %a } else { %a }" pp_expr cond pp_stmts then_
        pp_stmts else_
  | If (cond, then_, None) ->
      fprintf ppf "if (%a) { %a }" pp_expr cond pp_stmts then_
  | Return e -> fprintf ppf "return %a;" pp_expr e

and pp_stmts x = (pp_print_list ~pp_sep:pp_newl pp_stmt) x
