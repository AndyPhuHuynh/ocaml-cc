open Ocaml_cc

let print_string_error (source : Source.t) (e : Token_converter.string_error) :
    unit =
  match e with
  | InvalidEscape { seq; span } ->
      let msg = Printf.sprintf "Invalid seq: %s" seq in
      Diagnostics.emit_error (Diagnostics.from_span source span msg)

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
          match Token_converter.convert_token tok source_manager with
          | Ok tok -> print_endline (Token.to_string tok source_manager)
          | Error (StringError invalid_escapes) ->
              List.iter
                (fun e ->
                  print_string_error
                    (Source.get_source source_manager tok.span.source_id)
                    e)
                invalid_escapes)
        tokens
  | Error FileNotFound ->
      Diagnostics.emit_driver_error
        (Printf.sprintf "file not found: %s" !input_file)
  | Error (IOError msg) -> Diagnostics.emit_driver_error msg
