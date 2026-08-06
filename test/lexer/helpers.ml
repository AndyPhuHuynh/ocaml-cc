open Ocaml_cc

let test_lexer ?(verbose : bool = false) (contents : string) : unit =
  match Inspect.lex_all (Source.LoadString { name = "lex_test"; contents }) with
  | Ok (tokens, manager) ->
      let formatter =
        if verbose then Token.pp_list_verbose else Token.pp_list_compact
      in
      Format.printf "@[<v>%a@]@.@." (formatter manager) tokens
  | Error _ -> ()
