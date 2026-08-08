open Ocaml_cc

let test_lexer ?(verbose : bool = false) (contents : string) : unit =
  let result =
    Inspect.lex_all (Source.LoadString { name = "lex_test"; contents })
  in
  Inspect.print_result ~verbose result
