open Ocaml_cc

let run_test ~(initial_src : Types.test_content)
    (other_srcs : Types.test_content list)
    (execute_test : Source.load_file -> unit) : unit =
  let open Files in
  try
    let temp_dir = Filename.temp_dir "ocaml-cc-test-" "" in
    Fun.protect
      ~finally:(fun () -> rm_rf temp_dir)
      (fun () ->
        create_temp_files temp_dir other_srcs;
        let initial_src_fullpath = create_temp_file temp_dir initial_src in
        let initial_src : Source.load_file =
          {
            display_name = Some initial_src.name;
            filepath = initial_src_fullpath;
          }
        in
        execute_test initial_src)
  with Diagnostics.Exit code -> Printf.eprintf "Exit called with code %d" code
