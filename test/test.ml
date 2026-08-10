open Ocaml_cc

let test_lexer ?(verbose : bool = false) (contents : string) : unit =
  let initial_src : Types.test_content = { name = "lex_test"; contents } in
  Runner.run_test ~initial_src [] (fun initial_src ->
      let result = Inspect.lex_all initial_src in
      Inspect.print_result ~verbose result)

let test_pp ?(verbose : bool = false) ~(initial_src : Types.test_content)
    (other_srcs : Types.test_content list) : unit =
  Runner.run_test ~initial_src other_srcs (fun initial_src ->
      let result = Inspect.pp_all initial_src Source.empty_manager in
      Inspect.print_result ~verbose result)

let test_converter ?(verbose : bool = false) ~(initial_src : Types.test_content)
    (other_srcs : Types.test_content list) : unit =
  Runner.run_test ~initial_src other_srcs (fun initial_src ->
      let result = Inspect.convert_all initial_src Source.empty_manager in
      Inspect.print_result ~verbose result)
