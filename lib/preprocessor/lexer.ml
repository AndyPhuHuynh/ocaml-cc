type position = { pos : int; line : int; col : int }
type t = { source : string; position : position; start : position }

let is_whitespace (c : char) : bool =
  match c with ' ' | '\x09' .. '\x0d' -> true | _ -> false

let is_identifier_non_digit (c : char) : bool =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_digit (c : char) : bool = match c with '0' .. '9' -> true | _ -> false

let is_exponent_prefix (c : char) : bool =
  match c with 'e' | 'E' | 'p' | 'P' -> true | _ -> false

let default_pos : position = { pos = 0; line = 1; col = 1 }
let init source = { source; position = default_pos; start = default_pos }

let get_span_from_start (lexer : t) (start : position) : Token.span =
  { start = start.pos; finish = lexer.position.pos }

let get_span (lexer : t) : Token.span = get_span_from_start lexer lexer.start

let make_string_from_start_pos (lexer : t) (start : int) =
  String.sub lexer.source start (lexer.position.pos - start)

let make_string_from_current_bounds (lexer : t) : string =
  make_string_from_start_pos lexer lexer.start.pos

let is_at_end (lexer : t) : bool =
  lexer.position.pos >= String.length lexer.source

let at_index (lexer : t) : char option =
  if is_at_end lexer then None else Some lexer.source.[lexer.position.pos]

let advance_index (lexer : t) : t =
  match at_index lexer with
  | None -> lexer
  | Some '\n' ->
      {
        lexer with
        position =
          {
            pos = lexer.position.pos + 1;
            line = lexer.position.line + 1;
            col = 1;
          };
      }
  | _ ->
      {
        lexer with
        position =
          {
            pos = lexer.position.pos + 1;
            line = lexer.position.line;
            col = lexer.position.col + 1;
          };
      }

(** [splice_lines] will skip a backslash followed by any amount of whitespace
    and then a new line. If the backslash is not immediately followed by a
    newline, a warning is printed*)
let splice_lines (lexer : t) : t =
  let rec helper (original : t) (advanced : t) : t =
    match at_index advanced with
    | None | Some '\n' -> begin
        print_endline "Warning: whitespace found after trailing backslash";
        advance_index advanced
      end
    | Some c when is_whitespace c -> helper original (advance_index advanced)
    | _ -> original
  in

  match at_index lexer with
  | Some '\\' -> (
      let next_lexer = advance_index lexer in
      match at_index next_lexer with
      | None | Some '\n' -> advance_index next_lexer
      | Some c when is_whitespace c -> helper lexer (advance_index next_lexer)
      | _ -> lexer)
  | _ -> lexer

let peek_char (lexer : t) : char option =
  let lexer = splice_lines lexer in
  at_index lexer

let advance_char (lexer : t) : t =
  let lexer = splice_lines lexer in
  advance_index lexer

let make_token (kind : Token.kind) (lexer : t) : Token.t * t =
  let token : Token.t =
    {
      kind;
      span = get_span lexer;
      line = lexer.start.line;
      col = lexer.start.col;
    }
  in
  (token, lexer)

let rec skip_single_line_comment (lexer : t) : t * Token.t option =
  match peek_char lexer with
  | None -> (lexer, None)
  | Some '\n' -> skip_whitespace (advance_char lexer)
  | _ -> skip_single_line_comment (advance_char lexer)

and skip_multi_line_comment (lexer : t) : t * Token.t option =
  match peek_char lexer with
  | None ->
      let invalid_tok, lexer =
        make_token (Token.Invalid Token.UnterminatedComment) lexer
      in
      (lexer, Some invalid_tok)
  | Some '*' -> begin
      let next_lexer = advance_char lexer in
      match peek_char next_lexer with
      | Some '/' -> skip_whitespace (advance_char next_lexer)
      | _ -> skip_multi_line_comment (advance_char next_lexer)
    end
  | _ -> skip_multi_line_comment (advance_char lexer)

and skip_whitespace (lexer : t) : t * Token.t option =
  match peek_char lexer with
  | Some (' ' | '\t' | '\r') -> skip_whitespace (advance_char lexer)
  | Some '\n' -> begin
      let lexer = { lexer with start = lexer.position } in
      let tok, lexer = make_token Token.NewLine (advance_char lexer) in
      (lexer, Some tok)
    end
  | Some '/' -> begin
      let comment_pos = lexer.position in
      let next_lexer = advance_char lexer in
      match peek_char next_lexer with
      | Some '/' -> skip_single_line_comment (advance_char next_lexer)
      | Some '*' ->
          skip_multi_line_comment
            (advance_char { next_lexer with start = comment_pos })
      | _ -> (lexer, None)
    end
  | _ -> (lexer, None)

let lex_plus (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '+' -> make_token Token.PlusPlus (advance_char lexer)
  | Some '=' -> make_token Token.PlusEqual (advance_char lexer)
  | _ -> make_token Token.Plus lexer

let lex_minus (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '-' -> make_token Token.MinusMinus (advance_char lexer)
  | Some '=' -> make_token Token.MinusEqual (advance_char lexer)
  | Some '>' -> make_token Token.Arrow (advance_char lexer)
  | _ -> make_token Token.Minus lexer

let lex_star (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.StarEqual (advance_char lexer)
  | _ -> make_token Token.Star lexer

let lex_slash (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.SlashEqual (advance_char lexer)
  | _ -> make_token Token.Slash lexer

let lex_percent (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.PercentEqual (advance_char lexer)
  | _ -> make_token Token.Percent lexer

let lex_equal (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.EqualEqual (advance_char lexer)
  | _ -> make_token Token.Equal lexer

let lex_bang (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.BangEqual (advance_char lexer)
  | _ -> make_token Token.Bang lexer

let lex_less (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.LessEqual (advance_char lexer)
  | Some '<' -> begin
      let lexer = advance_char lexer in
      match peek_char lexer with
      | Some '=' -> make_token Token.LessLessEqual (advance_char lexer)
      | _ -> make_token Token.LessLess lexer
    end
  | _ -> make_token Token.Less lexer

let lex_greater (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.GreaterEqual (advance_char lexer)
  | Some '>' -> begin
      let lexer = advance_char lexer in
      match peek_char lexer with
      | Some '=' -> make_token Token.GreaterGreaterEqual (advance_char lexer)
      | _ -> make_token Token.GreaterGreater lexer
    end
  | _ -> make_token Token.Greater lexer

let lex_and (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '&' -> make_token Token.AndAnd (advance_char lexer)
  | Some '=' -> make_token Token.AndEqual (advance_char lexer)
  | _ -> make_token Token.And lexer

let lex_or (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '|' -> make_token Token.OrOr (advance_char lexer)
  | Some '=' -> make_token Token.OrEqual (advance_char lexer)
  | _ -> make_token Token.Or lexer

let lex_caret (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> make_token Token.CaretEqual (advance_char lexer)
  | _ -> make_token Token.Caret lexer

let lex_tilde (lexer : t) : Token.t * t = make_token Token.Tilde lexer

let lex_pp_number (lexer : t) : Token.t * t =
  let rec helper (lexer : t) (buf : Buffer.t) : Token.t * t =
    match peek_char lexer with
    | Some c when is_exponent_prefix c -> begin
        Buffer.add_char buf c;
        let after_prefix = advance_char lexer in
        match peek_char after_prefix with
        | Some sign when sign = '+' || sign = '-' ->
            Buffer.add_char buf sign;
            helper (advance_char after_prefix) buf
        | _ -> helper after_prefix buf
      end
    | Some c when is_identifier_non_digit c || is_digit c || c == '.' ->
        Buffer.add_char buf c;
        helper (advance_char lexer) buf
    | _ -> make_token (Token.PPNumber (Buffer.contents buf)) lexer
  in

  let buf = Buffer.create 16 in
  Buffer.add_char buf lexer.source.[lexer.start.pos];
  helper lexer buf

let lex_period (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '.' -> begin
      let next_lexer = advance_char lexer in
      match peek_char next_lexer with
      | Some '.' -> make_token Token.Ellipses (advance_char next_lexer)
      | _ -> make_token Token.Period lexer
    end
  | Some '0' .. '9' -> lex_pp_number (advance_char lexer)
  | _ -> make_token Token.Period lexer

let lex_hash (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '#' -> make_token Token.HashHash (advance_char lexer)
  | _ -> make_token Token.Hash lexer

let rec lex_identifier (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some ('_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9') ->
      lex_identifier (advance_char lexer)
  | _ ->
      make_token
        (Token.Identifier (make_string_from_current_bounds lexer))
        lexer

let lex_char_literal (lexer : t) : Token.t * t =
  let rec helper (lexer : t) (buf : Buffer.t) : Token.t * t =
    match peek_char lexer with
    | Some '\'' -> begin
        let lexer = advance_char lexer in
        make_token (Token.CharLiteral (Buffer.contents buf)) lexer
      end
    | None | Some '\n' -> begin
        let lexer = advance_char lexer in
        make_token (Token.Invalid Token.UnterminatedCharLiteral) lexer
      end
    | Some '\\' -> begin
        Buffer.add_char buf '\\';
        let next_lexer = advance_char lexer in
        match peek_char next_lexer with
        | Some '\'' ->
            Buffer.add_char buf '\'';
            helper (advance_char next_lexer) buf
        | _ -> helper next_lexer buf
      end
    | Some c ->
        Buffer.add_char buf c;
        helper (advance_char lexer) buf
  in

  helper lexer (Buffer.create 1)

let lex_string_literal (lexer : t) : Token.t * t =
  let rec helper (lexer : t) (buf : Buffer.t) : Token.t * t =
    match peek_char lexer with
    | Some '"' -> begin
        let lexer = advance_char lexer in
        make_token (Token.StringLiteral (Buffer.contents buf)) lexer
      end
    | None | Some '\n' -> begin
        let lexer = advance_char lexer in
        make_token (Token.Invalid Token.UnterminatedStringLiteral) lexer
      end
    | Some '\\' -> begin
        Buffer.add_char buf '\\';
        let next_lexer = advance_char lexer in
        match peek_char next_lexer with
        | Some '"' ->
            Buffer.add_char buf '"';
            helper (advance_char next_lexer) buf
        | _ -> helper next_lexer buf
      end
    | Some c ->
        Buffer.add_char buf c;
        helper (advance_char lexer) buf
  in
  helper lexer (Buffer.create 1)

let lex_token lexer =
  let lexer, tok = skip_whitespace lexer in
  let lexer = { lexer with start = lexer.position } in
  match tok with
  | Some token -> (token, lexer)
  | None ->
      begin match peek_char lexer with
      | None -> make_token Token.Eof lexer
      | Some c ->
          let lexer = advance_char lexer in
          begin match c with
          (* Operators *)
          | '+' -> lex_plus lexer
          | '-' -> lex_minus lexer
          | '*' -> lex_star lexer
          | '/' -> lex_slash lexer
          | '%' -> lex_percent lexer
          | '=' -> lex_equal lexer
          | '!' -> lex_bang lexer
          | '<' -> lex_less lexer
          | '>' -> lex_greater lexer
          | '&' -> lex_and lexer
          | '|' -> lex_or lexer
          | '^' -> lex_caret lexer
          | '~' -> lex_tilde lexer
          (* Punctuation *)
          | '(' -> make_token Token.LeftParen lexer
          | ')' -> make_token Token.RightParen lexer
          | '{' -> make_token Token.LeftBrace lexer
          | '}' -> make_token Token.RightBrace lexer
          | '[' -> make_token Token.LeftBracket lexer
          | ']' -> make_token Token.RightBracket lexer
          | ':' -> make_token Token.Colon lexer
          | ',' -> make_token Token.Comma lexer
          | ';' -> make_token Token.Semicolon lexer
          | '?' -> make_token Token.Question lexer
          | '#' -> lex_hash lexer
          | '.' -> lex_period lexer
          (* Literals *)
          | '_' | 'a' .. 'z' | 'A' .. 'Z' -> lex_identifier lexer
          | '0' .. '9' -> lex_pp_number lexer
          | '\'' -> lex_char_literal lexer
          | '"' -> lex_string_literal lexer
          | _ -> make_token (Token.Invalid (Token.InvalidChar c)) lexer
          end
      end

let lex_header_name lexer =
  let rec continue_lexing (lexer : t) (type_ : Token.header_type) : Token.t * t
      =
    let finish_lexing (lexer : t) (type_ : Token.header_type) =
      let filename = make_string_from_start_pos lexer (lexer.start.pos + 1) in
      make_token (Token.HeaderName { filename; type_ }) (advance_char lexer)
    in
    match peek_char lexer with
    | Some '>' when type_ = Token.NonLocal -> finish_lexing lexer Token.NonLocal
    | Some '"' when type_ = Token.Local -> finish_lexing lexer Token.Local
    | Some '\n' | None ->
        make_token (Token.Invalid Token.UnterminatedHeaderName)
          (advance_char lexer)
    | _ -> continue_lexing (advance_char lexer) type_
  in

  let lexer, tok = skip_whitespace lexer in
  match tok with
  | Some token -> (token, lexer)
  | None -> begin
      let lexer = { lexer with start = lexer.position } in
      match peek_char lexer with
      | Some '<' -> continue_lexing (advance_char lexer) Token.NonLocal
      | Some '"' -> continue_lexing (advance_char lexer) Token.Local
      | _ -> lex_token lexer
    end

let tokenize_all source =
  let rec helper (lexer : t) (acc : Token.t list) =
    let tok, lexer =
      match acc with
      | { kind = Token.Identifier "include"; _ }
        :: { kind = Token.Hash; _ }
        :: _ ->
          lex_header_name lexer
      | _ -> lex_token lexer
    in
    match tok with
    | { kind = Token.Eof } -> List.rev (tok :: acc)
    | _ -> helper lexer (tok :: acc)
  in
  helper (init source) []
