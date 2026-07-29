open Ocaml_cc

let () =
  let usage_msg = "ocaml-cc -i <input>" in
  let input_file = ref "" in
  let speclist = [ ("-i", Arg.Set_string input_file, "Input file") ] in

  Arg.parse speclist ignore usage_msg;
  if !input_file = "" then begin
    Diagnostics.emit_driver_error "no input files";
    exit 1
  end;

  match Preprocessor.tokenize_all !input_file with
  | Ok (tokens, source_manager) ->
      List.iter
        (fun tok ->
          print_endline
            (Token.to_string (Token_converter.convert_token tok) source_manager))
        tokens
  | Error FileNotFound ->
      Diagnostics.emit_driver_error
        (Printf.sprintf "file not found: %s" !input_file)
  | Error (IOError msg) -> Diagnostics.emit_driver_error msg
