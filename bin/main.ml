open Ocaml_cc

let print_string_error (source : Source.t) (e : Token_converter.string_error) :
    unit =
  match e with
  | InvalidEscape { seq_type; seq; span } -> (
      match seq_type with
      | SeqNormal ->
          let msg = Printf.sprintf "unknown escape sequence '%s'" seq in
          Diagnostics.emit_warning (Diagnostics.from_span source span msg))

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
