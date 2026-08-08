open Ocaml_cc

let test_lexer ?(verbose : bool = false) (contents : string) : unit =
  let result =
    Inspect.lex_all (Source.LoadString { name = "lex_test"; contents })
  in
  Inspect.print_result ~verbose result

let test_pp ?(verbose : bool = false) ~(initial_src : Source.load_string)
    (other_srcs : Source.load_string list) : unit =
  try
    let temp_dir = Filename.temp_dir "ocaml-cc-test-" "" in
    Fun.protect
      ~finally:(fun () -> Files.rm_rf temp_dir)
      (fun () ->
        Files.create_temp_files temp_dir other_srcs;
        let initial_src_fullpath =
          Files.create_temp_file temp_dir initial_src
        in
        let initial_src =
          Source.LoadFile
            {
              display_name = Some initial_src.name;
              filepath = initial_src_fullpath;
            }
        in
        let result = Inspect.pp_all initial_src Source.empty_manager in
        Inspect.print_result ~verbose result)
  with Diagnostics.Exit code -> Printf.eprintf "Exit called with code %d" code

let test_converter ?(verbose : bool = false) ~(initial_src : Source.load_string)
    (other_srcs : Source.load_string list) : unit =
  try
    let temp_dir = Filename.temp_dir "ocaml-cc-test-" "" in
    Fun.protect
      ~finally:(fun () -> Files.rm_rf temp_dir)
      (fun () ->
        Files.create_temp_files temp_dir other_srcs;
        let initial_src_fullpath =
          Files.create_temp_file temp_dir initial_src
        in
        let initial_src =
          Source.LoadFile
            {
              display_name = Some initial_src.name;
              filepath = initial_src_fullpath;
            }
        in
        let result = Inspect.convert_all initial_src Source.empty_manager in
        Inspect.print_result ~verbose result)
  with Diagnostics.Exit code -> Printf.eprintf "Exit called with code %d" code
