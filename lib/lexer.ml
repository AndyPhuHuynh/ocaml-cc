type position = { pos : int; line : int; col : int }
type t = { source : string; position : position; start : position }

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

let make_string_from_pos (lexer : t) : string =
  String.sub lexer.source lexer.start.pos (lexer.position.pos - lexer.start.pos)

let is_at_end (lexer : t) : bool =
  lexer.position.pos >= String.length lexer.source

let peek_char (lexer : t) : char option =
  if is_at_end lexer then None else Some lexer.source.[lexer.position.pos]

let advance_char (lexer : t) : t =
  match peek_char lexer with
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

let make_token (kind : Token.kind) (lexer : t) : Token.t * t =
  ( {
      kind;
      span = get_span lexer;
      line = lexer.start.line;
      col = lexer.start.col;
    },
    lexer )

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
  | Some (' ' | '\t' | '\n' | '\r') -> skip_whitespace (advance_char lexer)
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

let rec lex_pp_number (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some c when is_exponent_prefix c -> begin
      let after_prefix = advance_char lexer in
      match peek_char after_prefix with
      | Some ('+' | '-') -> lex_pp_number (advance_char after_prefix)
      | _ -> lex_pp_number after_prefix
    end
  | Some c when is_identifier_non_digit c || is_digit c || c == '.' ->
      lex_pp_number (advance_char lexer)
  | _ -> make_token (Token.PPNumber (make_string_from_pos lexer)) lexer

let advance_token lexer =
  let lexer, invalid_token = skip_whitespace lexer in
  match invalid_token with
  | Some token -> (token, lexer)
  | None ->
      begin match peek_char lexer with
      | None -> make_token Token.Eof lexer
      | Some c ->
          let lexer = { lexer with start = lexer.position } in
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
          | '.' -> make_token Token.Period lexer
          | '?' -> make_token Token.Question lexer
          (* Literals *)
          | '0' .. '9' -> lex_pp_number lexer
          | _ -> make_token (Token.Invalid (Token.InvalidChar c)) lexer
          end
      end

let tokenize_all source =
  let rec helper lexer acc =
    let tok, lexer = advance_token lexer in
    match tok with
    | { kind = Token.Eof } -> List.rev (tok :: acc)
    | _ -> helper lexer (tok :: acc)
  in
  helper (init source) []
