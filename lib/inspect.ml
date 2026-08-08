type inspect_result = (Token.t list * Source.manager, Source.load_error) result

let print_source_error (err : Source.load_error) : unit =
  match err with
  | Source.FileNotFound filepath -> Printf.eprintf "File not found: %s" filepath
  | Source.IOError msg -> Printf.eprintf "IOError: %s" msg

let print_result ?(verbose : bool = false) (result : inspect_result) : unit =
  match result with
  | Ok (tokens, manager) ->
      let formatter =
        if verbose then Token.pp_list_verbose else Token.pp_list_compact
      in
      Format.printf "@[<v>%a@]@.@." (formatter manager) tokens
  | Error err -> print_source_error err

let lex_all (load_type : Source.load_type) : inspect_result =
  let rec helper (lexer : Lexer.t) (acc : Token.t list) =
    let tok, lexer =
      match acc with
      | { kind = Token.Identifier "include"; _ }
        :: { kind = Token.Hash; _ }
        :: _ ->
          Lexer.lex_header_name lexer
      | _ -> Lexer.lex_token lexer
    in
    match tok with
    | { kind = Token.Eof } -> List.rev (tok :: acc)
    | _ -> helper lexer (tok :: acc)
  in

  match Source.load Source.empty_manager load_type with
  | Ok (manager, id, source) -> Ok (helper (Lexer.create id source) [], manager)
  | Error err -> Error err

let pp_all (load_type : Source.load_type) (manager : Source.manager) :
    inspect_result =
  let rec helper (pp : Preprocessor.t) (acc : Token.t list) :
      Token.t list * Source.manager =
    let tok, pp = Preprocessor.next_token pp in
    match tok.kind with
    | Eof -> (List.rev (tok :: acc), Preprocessor.get_source_manager pp)
    | _ -> helper pp (tok :: acc)
  in

  match Preprocessor.create_with_manager load_type manager with
  | Ok pp -> Ok (helper pp [])
  | Error err -> Error err

let convert_all (load_type : Source.load_type) (manager : Source.manager) :
    inspect_result =
  match pp_all load_type manager with
  | Ok (tokens, manager) -> begin
      let tokens =
        List.filter_map
          (fun (tok : Token.t) ->
            let source = Source.get_source manager tok.span.source_id in
            match Token_converter.convert_token tok manager with
            | Success tok -> Some tok
            | Recovered (tok, err) ->
                Token_converter.print_conversion_error source err;
                Some tok
            | Unrecoverable err ->
                Token_converter.print_conversion_error source err;
                None)
          tokens
      in
      Ok (tokens, manager)
    end
  | Error err -> Error err
