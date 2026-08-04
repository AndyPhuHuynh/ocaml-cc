let lex_all (source_id : Source.id) (source : Source.t) =
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
  helper (Lexer.create source_id source) []

let pp_all (filepath : string) :
    (Token.t list * Source.manager, Source.load_error) result =
  let rec helper (pp : Preprocessor.t) (acc : Token.t list) :
      Token.t list * Source.manager =
    let tok, pp = Preprocessor.next_token pp in
    match tok.kind with
    | Eof -> (List.rev (tok :: acc), Preprocessor.get_source_manager pp)
    | _ -> helper pp (tok :: acc)
  in

  match Preprocessor.create filepath with
  | Ok pp -> Ok (helper pp [])
  | Error err -> Error err
