open Mal.Parser.MenhirInterpreter

let rec repl ?ctx () =
  print_string "> ";
  try
    let rec multiline_helper lb =
      print_string "... ";
      let input = read_line () in
      let buf =
        Bytes.cat lb.Lexing.lex_buffer (Bytes.of_string ("\n  " ^ input))
      in
      {
        lb with
        Lexing.lex_buffer = buf;
        Lexing.lex_buffer_len = Bytes.length buf;
      }
    and helper chkpt lb =
      match chkpt with
      | InputNeeded _ ->
          let lb =
            if
              lb.Lexing.lex_curr_pos == lb.Lexing.lex_buffer_len
              && not (acceptable chkpt Mal.Parser.EOF lb.Lexing.lex_curr_p)
            then multiline_helper lb
            else lb
          in
          helper
            (offer chkpt
               ( Mal.Lexer.token lb,
                 Lexing.lexeme_start_p lb,
                 Lexing.lexeme_end_p lb ))
            lb
      | Shifting (_, _, _) -> helper (resume chkpt) lb
      | AboutToReduce (_, _) -> helper (resume chkpt) lb
      | HandlingError _ -> helper (resume chkpt) lb
      | Accepted prg -> prg
      | Rejected -> raise (Mal.Utils.ParseError (Lexing.lexeme_start_p lb))
    in
    let lb = Lexing.from_string (read_line ()) in
    Lexing.set_filename lb "<stdin>";
    let program = helper (Mal.Parser.Incremental.toplevel lb.lex_start_p) lb in
    Format.printf "%a" (Format.pp_print_list Mal.Pp.pp_stmt) program;
    Format.print_newline ();
    let ctx =
      match ctx with
      | None -> Mal.Codegen.codegen program
      | Some ctx -> Mal.Codegen.codegen ~ctx program
    in
    Llvm.dump_module ctx.Mal.Codegen.llvm_module;
    repl ~ctx ()
  with
  | End_of_file -> exit 0
  | Mal.Utils.ParseError pos ->
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
