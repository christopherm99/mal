let () =
  match Array.length Sys.argv with
  | 1 -> print_string "must specify input file\n"; exit 1
  | 2 -> 
      let fn = Sys.argv.(1) in
      Printf.printf "compiling: %s\n" fn;
      let ic = open_in fn in
      let lb = Lexing.from_channel ic in
      Lexing.set_filename lb fn;
      let program = Mal.Parser.toplevel Mal.Lexer.token lb in
      (*Format.printf "%a" (Format.pp_print_list Mal.Pp.pp_stmt) program;
      print_newline ();*)
      ignore @@ Llvm_bitwriter.write_bitcode_file (Mal.Codegen.codegen program).Mal.Codegen.llvm_module "a.out"
  | _ -> print_string "only accepts a single input\n"; exit 1
