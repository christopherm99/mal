open Mal.Parser.MenhirInterpreter

(* let rec repl_multiline prev_input =
     print_string "... ";
     try
       let input = prev_input ^ read_line () in
       try
         let program = Mal.Utils.parse_program input in
         ignore @@ List.map Mal.Codegen.codegen_statement program;
         Llvm.dump_module Mal.Codegen.the_module;
         repl ()
       with
       | Mal.Utils.ParseError pos ->
         if pos.pos_lnum = 1 then repl_multiline input
         else
           Printf.printf "ParseError at \"%s\" line %d column %d\n" pos.pos_fname
             pos.pos_lnum pos.pos_bol;
         repl ()
       | Mal.Utils.LexError (pos, msg) ->
           Printf.printf "LexError(%s) at \"%s\" line %d column %d\n" msg pos.pos_fname pos.pos_lnum pos.pos_bol;
           repl ()
     with End_of_file ->
       print_string "Input Cancelled\n";
       repl ()
   and *)
let rec repl () =
  print_string "> ";
  try
    let input = read_line () in
    let lb = Lexing.from_string input in
    let rec helper chkpt lb =
      match chkpt with
      | InputNeeded _ ->
          helper
            (offer chkpt
               ( Mal.Lexer.token lb,
                 Lexing.lexeme_start_p lb,
                 Lexing.lexeme_end_p lb ))
            lb
      | Shifting (_, _, _) -> helper (resume chkpt) lb
      | AboutToReduce (_, _) -> helper (resume chkpt) lb
      | HandlingError _ ->
          print_string "handling error";
          helper (resume chkpt) lb
      | Accepted prg -> prg
      | Rejected -> raise (Mal.Utils.ParseError (Lexing.lexeme_start_p lb))
    in
    let program = helper (Mal.Parser.Incremental.toplevel lb.lex_start_p) lb in
    Format.printf "%a" (Format.pp_print_list Mal.Pp.pp_stmt) program;
    Format.print_newline ();
    ignore @@ List.map Mal.Codegen.codegen_statement program;
    Llvm.dump_module Mal.Codegen.the_module;
    repl ()
  with
  | End_of_file -> exit 0
  | Mal.Utils.ParseError pos ->
      (*if pos.pos_lnum = 1 then repl_multiline input
        else*)
      Printf.printf "ParseError at \"%s\" line %d column %d\n" pos.pos_fname
        pos.pos_lnum pos.pos_bol;
      repl ()
  | Mal.Utils.LexError (pos, _) ->
      Printf.printf "LexError at \"%s\" column %d\n" pos.pos_fname pos.pos_bol;
      repl ()
  | Mal.Codegen.Error msg ->
      Printf.printf "CodegenError: %s\n" msg;
      repl ()

let () = repl ()
