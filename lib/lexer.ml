type t = { source : string; position : int; line : int; col : int }

let init source = { source; position = 0; line = 1; col = 1 }
let is_at_end (lexer : t) : bool = lexer.position >= String.length lexer.source

let peek_char (lexer : t) : char option =
  if is_at_end lexer then None else Some lexer.source.[lexer.position]

let get_lc (lexer : t) : int * int = (lexer.line, lexer.col)

let make_token (kind : Token.kind) ((line, col) : int * int) : Token.t =
  { kind; line; col }

let advance_char (lexer : t) : t =
  match peek_char lexer with
  | None -> lexer
  | Some '\n' ->
      {
        lexer with
        position = lexer.position + 1;
        line = lexer.line + 1;
        col = 1;
      }
  | _ -> { lexer with position = lexer.position + 1; col = lexer.col + 1 }

let rec skip_single_line_comment (lexer : t) : t * Token.t option =
  match peek_char lexer with
  | None -> (lexer, None)
  | Some '\n' -> skip_whitespace (advance_char lexer)
  | _ -> skip_single_line_comment (advance_char lexer)

and skip_multi_line_comment (lexer : t) (start : int * int) : t * Token.t option
    =
  match peek_char lexer with
  | None ->
      (lexer, Some (make_token (Token.Invalid Token.UnterminatedComment) start))
  | Some '*' -> begin
      let next_lexer = advance_char lexer in
      match peek_char next_lexer with
      | Some '/' -> skip_whitespace (advance_char next_lexer)
      | _ -> skip_multi_line_comment (advance_char next_lexer) start
    end
  | _ -> skip_multi_line_comment (advance_char lexer) start

and skip_whitespace (lexer : t) : t * Token.t option =
  match peek_char lexer with
  | Some (' ' | '\t' | '\n' | '\r') -> skip_whitespace (advance_char lexer)
  | Some '/' -> begin
      let start = get_lc lexer in
      let next_lexer = advance_char lexer in
      match peek_char next_lexer with
      | Some '/' -> skip_single_line_comment (advance_char next_lexer)
      | Some '*' -> skip_multi_line_comment (advance_char next_lexer) start
      | _ -> (lexer, None)
    end
  | _ -> (lexer, None)

let lex_plus (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '+' -> (make_token Token.PlusPlus start, advance_char lexer)
  | Some '=' -> (make_token Token.PlusEqual start, advance_char lexer)
  | _ -> (make_token Token.Plus start, lexer)

let lex_minus (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '-' -> (make_token Token.MinusMinus start, advance_char lexer)
  | Some '=' -> (make_token Token.MinusEqual start, advance_char lexer)
  | Some '>' -> (make_token Token.Arrow start, advance_char lexer)
  | _ -> (make_token Token.Minus start, lexer)

let lex_star (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.StarEqual start, advance_char lexer)
  | _ -> (make_token Token.Star start, lexer)

let lex_slash (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.SlashEqual start, advance_char lexer)
  | _ -> (make_token Token.Slash start, lexer)

let lex_percent (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.PercentEqual start, advance_char lexer)
  | _ -> (make_token Token.Percent start, lexer)

let lex_equal (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.EqualEqual start, advance_char lexer)
  | _ -> (make_token Token.Equal start, lexer)

let lex_bang (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.BangEqual start, advance_char lexer)
  | _ -> (make_token Token.Bang start, lexer)

let lex_less (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.LessEqual start, advance_char lexer)
  | Some '<' -> begin
      let lexer = advance_char lexer in
      match peek_char lexer with
      | Some '=' -> (make_token Token.LessLessEqual start, advance_char lexer)
      | _ -> (make_token Token.LessLess start, lexer)
    end
  | _ -> (make_token Token.Less start, lexer)

let lex_greater (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.GreaterEqual start, advance_char lexer)
  | Some '>' -> begin
      let lexer = advance_char lexer in
      match peek_char lexer with
      | Some '=' ->
          (make_token Token.GreaterGreaterEqual start, advance_char lexer)
      | _ -> (make_token Token.GreaterGreater start, lexer)
    end
  | _ -> (make_token Token.Greater start, lexer)

let lex_and (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '&' -> (make_token Token.AndAnd start, advance_char lexer)
  | Some '=' -> (make_token Token.AndEqual start, advance_char lexer)
  | _ -> (make_token Token.And start, lexer)

let lex_or (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '|' -> (make_token Token.OrOr start, advance_char lexer)
  | Some '=' -> (make_token Token.OrEqual start, advance_char lexer)
  | _ -> (make_token Token.Or start, lexer)

let lex_caret (lexer : t) (start : int * int) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (make_token Token.CaretEqual start, advance_char lexer)
  | _ -> (make_token Token.Caret start, lexer)

let lex_tilde (lexer : t) (start : int * int) : Token.t * t =
  (make_token Token.Tilde start, lexer)

let advance_token lexer =
  let lexer, invalid_token = skip_whitespace lexer in
  match invalid_token with
  | Some token -> (token, lexer)
  | None -> (
      match peek_char lexer with
      | None -> (make_token Token.Eof (get_lc lexer), lexer)
      | Some c -> (
          let start = get_lc lexer in
          let lexer = advance_char lexer in
          match c with
          (* Operators *)
          | '+' -> lex_plus lexer start
          | '-' -> lex_minus lexer start
          | '*' -> lex_star lexer start
          | '/' -> lex_slash lexer start
          | '%' -> lex_percent lexer start
          | '=' -> lex_equal lexer start
          | '!' -> lex_bang lexer start
          | '<' -> lex_less lexer start
          | '>' -> lex_greater lexer start
          | '&' -> lex_and lexer start
          | '|' -> lex_or lexer start
          | '^' -> lex_caret lexer start
          | '~' -> lex_tilde lexer start
          (* Punctuation *)
          | '(' -> (make_token Token.LeftParen start, lexer)
          | ')' -> (make_token Token.RightParen start, lexer)
          | '{' -> (make_token Token.LeftBrace start, lexer)
          | '}' -> (make_token Token.RightBrace start, lexer)
          | '[' -> (make_token Token.LeftBracket start, lexer)
          | ']' -> (make_token Token.RightBracket start, lexer)
          | ':' -> (make_token Token.Colon start, lexer)
          | ',' -> (make_token Token.Comma start, lexer)
          | ';' -> (make_token Token.Semicolon start, lexer)
          | '.' -> (make_token Token.Period start, lexer)
          | '?' -> (make_token Token.Question start, lexer)
          | _ -> (make_token Token.Eof start, lexer)))

let tokenize_all source =
  let rec helper lexer acc =
    let tok, lexer = advance_token lexer in
    match tok with
    | { kind = Token.Eof } -> List.rev (tok :: acc)
    | _ -> helper lexer (tok :: acc)
  in
  helper (init source) []
