(lang dune 3.15)

(name mal)

(generate_opam_files true)

(source
 (github christopherm99/mal))

(authors "Christopher Milan")

(maintainers "Christopher Milan")

(license MIT)

(documentation https://github.com/christopherm99/mal)

(package
 (name mal)
 (synopsis "Mal, bad... in the latin")
 (description "A programming language")
 (depends ocaml dune menhir llvm ocamlformat (alcotest :with-test)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
