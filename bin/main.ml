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

  match Inspect.lex_all { display_name = None; filepath = !input_file } with
  | Ok (tokens, manager) -> begin
      List.iter
        (fun tok -> Format.printf "%a@.@." (Token.pp_verbose manager) tok)
        tokens
    end
  | Error (FileNotFound filepath) ->
      Diagnostics.emit_driver_error
        (Printf.sprintf "file not found: %s" filepath)
  | Error (IOError msg) -> Diagnostics.emit_driver_error msg

(* match Inspect.pp_all !input_file with *)
(* | Ok (tokens, source_manager) -> begin *)
(*     List.iter *)
(*       (fun (tok : Token.t) -> *)
(*         let source = Source.get_source source_manager tok.span.source_id in *)
(*         match Token_converter.convert_token tok source_manager with *)
(*         | Success tok -> Format.printf "%a@.@." (Token.pp source_manager) tok *)
(*         | Recovered (tok, error) -> *)
(*             Format.printf "%a@.@." (Token.pp source_manager) tok; *)
(*             print_conversion_error source error *)
(*         | Unrecoverable error -> print_conversion_error source error) *)
(*       tokens *)
(*   end *)
(* | Error FileNotFound -> *)
(*     Diagnostics.emit_driver_error *)
(*       (Printf.sprintf "file not found: %s" !input_file) *)
(* | Error (IOError msg) -> Diagnostics.emit_driver_error msg *)
