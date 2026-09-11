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

  match
    Parser.create
      { display_name = None; filepath = !input_file }
      (Diagnostics.create_engine ())
  with
  | Ok parser -> begin Parser.parse_translation_unit parser end
  | Error (FileNotFound filepath) ->
      Diagnostics.emit_driver_error
        (Printf.sprintf "file not found: %s" filepath)
  | Error (IOError msg) -> Diagnostics.emit_driver_error msg

(* match Inspect.lex_all { display_name = None; filepath = !input_file } with *)
(* | Ok (tokens, manager, _) -> begin *)
(*     List.iter *)
(*       (fun tok -> Format.printf "%a@.@." (Token.pp_verbose manager) tok) *)
(*       tokens *)
(*   end *)
(* | Error (FileNotFound filepath) -> *)
(*     Diagnostics.emit_driver_error *)
(*       (Printf.sprintf "file not found: %s" filepath) *)
(* | Error (IOError msg) -> Diagnostics.emit_driver_error msg *)
