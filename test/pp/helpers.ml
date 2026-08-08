open Ocaml_cc

let rec rm_rf path =
  match Sys.is_directory path with
  | true ->
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
  | false -> Sys.remove path

let create_temp_file (base_dir : string) (src : Source.load_string) : string =
  let fullpath = Filename.concat base_dir src.name in
  let oc = open_out_bin fullpath in
  try
    output_string oc src.contents;
    close_out oc;
    fullpath
  with e ->
    close_out_noerr oc;
    raise e

let create_temp_files (base_dir : string) (srcs : Source.load_string list) :
    unit =
  List.iter
    (fun (src : Source.load_string) ->
      begin
        ignore (create_temp_file base_dir src)
      end)
    srcs

let test_pp ?(verbose : bool = false) ~(initial_src : Source.load_string)
    (other_srcs : Source.load_string list) : unit =
  try
    let temp_dir = Filename.temp_dir "ocaml-cc-test-" "" in
    Fun.protect
      ~finally:(fun () -> rm_rf temp_dir)
      (fun () ->
        create_temp_files temp_dir other_srcs;
        let initial_src_fullpath = create_temp_file temp_dir initial_src in
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
