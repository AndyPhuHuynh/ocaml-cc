type t = { source : string; position : int; line : int; col : int }

let init source = { source; position = 0; line = 1; col = 1 }
let is_at_end (lexer : t) : bool = lexer.position >= String.length lexer.source

let peek_char (lexer : t) : char option =
  if is_at_end lexer then None else Some lexer.source.[lexer.position]

let advance_char (lexer : t) : t =
  match peek_char lexer with
  | Some '\n' ->
      {
        lexer with
        position = lexer.position + 1;
        line = lexer.line + 1;
        col = 1;
      }
  | _ -> { lexer with position = lexer.position + 1; col = lexer.col + 1 }

let rec skip_whitespace (lexer : t) =
  match peek_char lexer with
  | Some ' ' | Some '\t' | Some '\n' | Some '\r' | Some '\012' ->
      skip_whitespace (advance_char lexer)
  | _ -> lexer

let get_prev_lc (lexer : t) = (lexer.line, lexer.col - 1)

let make_token (kind : Token.kind) ((line, col) : int * int) : Token.t =
  { kind; line; col }

let lex_plus (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '+' -> (make_token Token.PlusPlus start, advance_char lexer)
  | Some '=' -> (make_token Token.PlusEqual start, advance_char lexer)
  | _ -> (make_token Token.Plus start, lexer)

let lex_minus (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '-' -> (make_token Token.MinusMinus start, advance_char lexer)
  | Some '=' -> (make_token Token.MinusEqual start, advance_char lexer)
  | Some '>' -> (make_token Token.Arrow start, advance_char lexer)
  | _ -> (make_token Token.Minus start, lexer)

let lex_star (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '=' -> (make_token Token.StarEqual start, advance_char lexer)
  | _ -> (make_token Token.Star start, lexer)

let lex_slash (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '=' -> (make_token Token.SlashEqual start, advance_char lexer)
  | _ -> (make_token Token.Slash start, lexer)

let lex_percent (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '=' -> (make_token Token.PercentEqual start, advance_char lexer)
  | _ -> (make_token Token.Percent start, lexer)

let lex_equal (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '=' -> (make_token Token.EqualEqual start, advance_char lexer)
  | _ -> (make_token Token.Equal start, lexer)

let lex_bang (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '=' -> (make_token Token.BangEqual start, advance_char lexer)
  | _ -> (make_token Token.Bang start, lexer)

let lex_less (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '=' -> (make_token Token.LessEqual start, advance_char lexer)
  | Some '<' -> begin
      let lexer = advance_char lexer in
      match peek_char lexer with
      | Some '=' -> (make_token Token.LessLessEqual start, advance_char lexer)
      | _ -> (make_token Token.LessLess start, lexer)
    end
  | _ -> (make_token Token.Less start, lexer)

let lex_greater (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
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

let lex_and (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '&' -> (make_token Token.AndAnd start, advance_char lexer)
  | Some '=' -> (make_token Token.AndEqual start, advance_char lexer)
  | _ -> (make_token Token.And start, lexer)

let lex_or (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '|' -> (make_token Token.OrOr start, advance_char lexer)
  | Some '=' -> (make_token Token.OrEqual start, advance_char lexer)
  | _ -> (make_token Token.Or start, lexer)

let lex_caret (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  match peek_char lexer with
  | Some '=' -> (make_token Token.CaretEqual start, advance_char lexer)
  | _ -> (make_token Token.Caret start, lexer)

let lex_tilde (lexer : t) : Token.t * t =
  let start = get_prev_lc lexer in
  (make_token Token.Tilde start, lexer)

let advance_token lexer =
  let lexer = skip_whitespace lexer in
  match peek_char lexer with
  | None -> (make_token Token.Eof (get_prev_lc lexer), lexer)
  | Some c -> (
      let lexer = advance_char lexer in
      let start = get_prev_lc lexer in
      match c with
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
      | _ -> (make_token Token.Eof start, lexer))

let tokenize_all source =
  let rec helper lexer acc =
    let tok, lexer = advance_token lexer in
    match tok with
    | { kind = Token.Eof } -> List.rev (tok :: acc)
    | _ -> helper lexer (tok :: acc)
  in
  helper (init source) []
