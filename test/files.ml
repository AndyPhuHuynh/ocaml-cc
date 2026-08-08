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
