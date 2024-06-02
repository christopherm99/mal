let statement = Alcotest.testable Mal.Pp.pp_stmt ( = )

let test_simple_return () =
  let open Mal.Syntax in
  Alcotest.(check (list statement))
    "same parse tree"
    [ Function (Prototype ("test", []), [ Return (Number 0) ]) ]
    (Mal.Parser.toplevel Mal.Lexer.token
       (Lexing.from_string "fn test() { return 0; }"))

let () =
  let open Alcotest in
  run "Parser"
    [ ("simple-fn", [ test_case "Only return" `Quick test_simple_return ]) ]
