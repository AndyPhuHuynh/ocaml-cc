(lang dune 3.24)

(name ocaml-cc)

(generate_opam_files true)

(package
 (name ocaml-cc)
 (synopsis "A C compiler written in OCaml")
 (description "An educational C Compiler written in OCaml")
 (depends 
   ocaml
   dune
   zarith))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
