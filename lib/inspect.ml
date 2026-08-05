type inspect_result = (Token.t list * Source.manager, Source.load_error) result

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

let pp_all (load_type : Source.load_type) : inspect_result =
  let rec helper (pp : Preprocessor.t) (acc : Token.t list) :
      Token.t list * Source.manager =
    let tok, pp = Preprocessor.next_token pp in
    match tok.kind with
    | Eof -> (List.rev (tok :: acc), Preprocessor.get_source_manager pp)
    | _ -> helper pp (tok :: acc)
  in

  match Preprocessor.create load_type with
  | Ok pp -> Ok (helper pp [])
  | Error err -> Error err
