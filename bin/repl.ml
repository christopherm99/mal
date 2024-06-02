let rec repl_multiline prev_input =
  print_string "... ";
  let input = prev_input ^ read_line () in
  try
    let program = Mal.Utils.parse_program input in
    ignore @@ List.map Mal.Codegen.codegen_statement program;
    Llvm.dump_module Mal.Codegen.the_module;
    repl ()
  with Mal.Utils.ParseError pos ->
    if pos.pos_lnum = 1 then repl_multiline input
    else
      Printf.printf "ParseError at \"%s\" line %d column %d" pos.pos_fname
        pos.pos_lnum pos.pos_bol;
    repl ()

and repl () =
  print_string "> ";
  let input = read_line () in
  try
    let program = Mal.Utils.parse_program input in
    ignore @@ List.map Mal.Codegen.codegen_statement program;
    Llvm.dump_module Mal.Codegen.the_module;
    repl ()
  with Mal.Utils.ParseError pos ->
    if pos.pos_lnum = 1 then repl_multiline input
    else
      Printf.printf "ParseError at \"%s\" column %d" pos.pos_fname pos.pos_bol;
    repl ()

let () = repl ()
