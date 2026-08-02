open Ocaml_cc

let print_string_error (source : Source.t) (e : Token_converter.string_error) :
    unit =
  let emit_fn, msg =
    match e.seq_type with
    | SeqNormal ->
        ( Diagnostics.emit_warning,
          Printf.sprintf "unknown escape sequence '%s'" e.seq )
    | HexNoDigits ->
        (Diagnostics.emit_error, "\\x used with no following hex digits")
    | HexTooLarge -> (Diagnostics.emit_error, "hex escape sequence out of range")
  in
  emit_fn (Diagnostics.from_span source e.span msg)

let print_conversion_error (source : Source.t)
    (e : Token_converter.conversion_error) : unit =
  match e with
  | StringError errors ->
      List.iter (fun e -> print_string_error source e) errors

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
  | Ok (tokens, source_manager) -> begin
      List.iter
        (fun (tok : Token.t) ->
          let source = Source.get_source source_manager tok.span.source_id in
          match Token_converter.convert_token tok source_manager with
          | Success tok -> print_endline (Token.to_string tok source_manager)
          | Recovered (tok, error) ->
              print_endline (Token.to_string tok source_manager);
              print_conversion_error source error
          | Unrecoverable error -> print_conversion_error source error)
        tokens
    end
  | Error FileNotFound ->
      Diagnostics.emit_driver_error
        (Printf.sprintf "file not found: %s" !input_file)
  | Error (IOError msg) -> Diagnostics.emit_driver_error msg
