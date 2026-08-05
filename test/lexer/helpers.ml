open Ocaml_cc

let test_lexer (contents : string) : unit =
  match Inspect.lex_all (Source.LoadString { name = "lex_test"; contents }) with
  | Ok (tokens, manager) ->
      Format.printf "@[<v>%a@]" (Token.pp_list manager) tokens
  | Error _ -> ()
