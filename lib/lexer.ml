type t = { source : string; position : int }

let init source = { source; position = 0 }
let is_at_end (lexer : t) : bool = lexer.position >= String.length lexer.source

let peek_char (lexer : t) : char option =
  if is_at_end lexer then None else Some lexer.source.[lexer.position]

let advance_char (lexer : t) : t = { lexer with position = lexer.position + 1 }

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let rec skip_whitespace (lexer : t) =
  match peek_char lexer with
  | None -> lexer
  | Some c ->
      if is_whitespace c then skip_whitespace (advance_char lexer) else lexer

let lex_plus (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '+' -> (Token.PlusPlus, advance_char lexer)
  | Some '=' -> (Token.PlusEqual, advance_char lexer)
  | _ -> (Token.Plus, lexer)

let lex_minus (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '-' -> (Token.MinusMinus, advance_char lexer)
  | Some '=' -> (Token.MinusEqual, advance_char lexer)
  | Some '>' -> (Token.Arrow, advance_char lexer)
  | _ -> (Token.Minus, lexer)

let lex_star (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.StarEqual, advance_char lexer)
  | _ -> (Token.Star, lexer)

let lex_slash (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.SlashEqual, advance_char lexer)
  | _ -> (Token.Slash, lexer)

let lex_percent (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.PercentEqual, advance_char lexer)
  | _ -> (Token.Percent, lexer)

let lex_equal (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.EqualEqual, advance_char lexer)
  | _ -> (Token.Equal, lexer)

let lex_bang (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.BangEqual, advance_char lexer)
  | _ -> (Token.Bang, lexer)

let lex_less (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.LessEqual, advance_char lexer)
  | Some '<' -> begin
      let lexer = advance_char lexer in
      match peek_char lexer with
      | Some '=' -> (Token.LessLessEqual, advance_char lexer)
      | _ -> (Token.LessLess, lexer)
    end
  | _ -> (Token.Less, lexer)

let lex_greater (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.GreaterEqual, advance_char lexer)
  | Some '>' -> begin
      let lexer = advance_char lexer in
      match peek_char lexer with
      | Some '=' -> (Token.GreaterGreaterEqual, advance_char lexer)
      | _ -> (Token.GreaterGreater, lexer)
    end
  | _ -> (Token.Greater, lexer)

let lex_and (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '&' -> (Token.AndAnd, advance_char lexer)
  | Some '=' -> (Token.AndEqual, advance_char lexer)
  | _ -> (Token.And, lexer)

let lex_or (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '|' -> (Token.OrOr, advance_char lexer)
  | Some '=' -> (Token.OrEqual, advance_char lexer)
  | _ -> (Token.Or, lexer)

let lex_caret (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '=' -> (Token.CaretEqual, advance_char lexer)
  | _ -> (Token.Caret, lexer)

let lex_tilde (lexer : t) : Token.t * t = (Token.Tilde, lexer)

let advance_token lexer =
  let lexer = skip_whitespace lexer in
  match peek_char lexer with
  | None -> (Token.Eof, lexer)
  | Some c -> (
      let lexer = advance_char lexer in
      match c with
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
      | _ -> (Token.Eof, lexer))

let tokenize_all source =
  let rec helper lexer acc =
    let tok, lexer = advance_token lexer in
    match tok with
    | Token.Eof -> List.rev (tok :: acc)
    | _ -> helper lexer (tok :: acc)
  in
  helper (init source) []
