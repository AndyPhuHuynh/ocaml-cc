type t = {
  source_id : Source.id;
  source : Source.t;
  position : Source.pos;
  start : Source.pos;
  next_token_starts_line : bool;
}

let is_whitespace (c : char) : bool =
  match c with ' ' | '\x09' .. '\x0d' -> true | _ -> false

let is_identifier_non_digit (c : char) : bool =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_digit (c : char) : bool = match c with '0' .. '9' -> true | _ -> false

let is_exponent_prefix (c : char) : bool =
  match c with 'e' | 'E' | 'p' | 'P' -> true | _ -> false

let create (source_id : Source.id) (source : Source.t) : t =
  {
    source_id;
    source;
    position = Source.default_pos;
    start = Source.default_pos;
    next_token_starts_line = true;
  }

let get_span (lexer : t) : Source.span =
  {
    source_id = lexer.source_id;
    start = lexer.start.index;
    length = lexer.position.index - lexer.start.index;
  }

let make_string_from_start_pos (lexer : t) (start : int) =
  String.sub lexer.source.contents start (lexer.position.index - start)

let make_string_from_current_bounds (lexer : t) : string =
  make_string_from_start_pos lexer lexer.start.index

let is_at_end (lexer : t) : bool =
  lexer.position.index >= String.length lexer.source.contents

let at_index (lexer : t) : char option =
  if is_at_end lexer then None
  else Some lexer.source.contents.[lexer.position.index]

let advance_index (lexer : t) : t =
  match at_index lexer with
  | None -> lexer
  | Some '\n' ->
      {
        lexer with
        position =
          {
            index = lexer.position.index + 1;
            loc = { line = lexer.position.loc.line + 1; col = 1 };
          };
      }
  | _ ->
      {
        lexer with
        position =
          {
            index = lexer.position.index + 1;
            loc =
              {
                line = lexer.position.loc.line;
                col = lexer.position.loc.col + 1;
              };
          };
      }

type splice = { lexer : t; whitespace_separated : bool; loc : Source.loc }
type splice_result = NoSplice | Splice of splice

(** [find_line_splice] will check for backslash followed by any amount of
    whitespace and then a new line. *)
let find_line_splice (lexer : t) : splice_result =
  let rec helper (original : t) (advanced : t) : splice_result =
    match at_index advanced with
    | None | Some '\n' -> begin
        let advanced = advance_index advanced in
        Splice
          {
            lexer = advanced;
            whitespace_separated = true;
            loc = original.position.loc;
          }
      end
    | Some c when is_whitespace c -> helper original (advance_index advanced)
    | _ -> NoSplice
  in

  match at_index lexer with
  | Some '\\' -> (
      let next_lexer = advance_index lexer in
      match at_index next_lexer with
      | None | Some '\n' ->
          Splice
            {
              lexer = advance_index next_lexer;
              whitespace_separated = false;
              loc = lexer.position.loc;
            }
      | Some c when is_whitespace c -> helper lexer (advance_index next_lexer)
      | _ -> NoSplice)
  | _ -> NoSplice

let find_line_splice_sequence (lexer : t) : splice list =
  let rec helper (lexer : t) (acc : splice list) =
    match find_line_splice lexer with
    | NoSplice -> List.rev acc
    | Splice result -> helper result.lexer (result :: acc)
  in
  helper lexer []

let emit_splice_diagnostic (splice : splice) : unit =
  Diagnostics.emit_warning
    (Diagnostics.at splice.lexer.source splice.loc
       "backslash and newline separated by whitespace")

let rec last_elem = function
  | [] -> failwith "last_elem called with empty list"
  | [ x ] -> x
  | x :: xs -> last_elem xs

type char_view = { char : char; pos : Source.pos; splice_encountered : bool }

let peek_char_view (lexer : t) : char_view option =
  let lexer, splice_encountered =
    match find_line_splice_sequence lexer with
    | [] -> (lexer, false)
    | xs -> ((last_elem xs).lexer, true)
  in

  let c = at_index lexer in
  match c with
  | None -> None
  | Some c -> Some { char = c; pos = lexer.position; splice_encountered }

let peek_char (lexer : t) : char option =
  let lexer =
    match find_line_splice_sequence lexer with
    | [] -> lexer
    | xs -> (last_elem xs).lexer
  in
  at_index lexer

let advance_char (lexer : t) : t =
  let rec iter_splices (list : splice list) : t option =
    match list with
    | [] -> None
    | [ x ] ->
        if x.whitespace_separated then begin
          emit_splice_diagnostic x
        end;
        Some x.lexer
    | x :: xs ->
        if x.whitespace_separated then begin
          emit_splice_diagnostic x
        end;
        iter_splices xs
  in

  let lexer =
    match iter_splices (find_line_splice_sequence lexer) with
    | None -> lexer
    | Some lexer -> lexer
  in

  advance_index lexer

type string_builder = {
  buffer : Buffer.t;
  mutable positions : Source.string_pos list;
}

let sb_create (buffer_size : int) : string_builder =
  { buffer = Buffer.create buffer_size; positions = [] }

let sb_add_char (sb : string_builder) (char_view : char_view) : unit =
  Buffer.add_char sb.buffer char_view.char;
  match sb.positions with
  | [] ->
      sb.positions <-
        [ { index = Buffer.length sb.buffer - 1; loc = char_view.pos.loc } ]
  | _ ->
      if char_view.splice_encountered then begin
        let index = Buffer.length sb.buffer - 1 in
        sb.positions <- { index; loc = char_view.pos.loc } :: sb.positions
      end

let make_token (kind : Token.kind) (lexer : t) : Token.t * t =
  let is_at_line_start = lexer.next_token_starts_line in
  let lexer = { lexer with next_token_starts_line = kind = Token.NewLine } in
  let token : Token.t =
    { kind; span = get_span lexer; loc = lexer.start.loc; is_at_line_start }
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

let lex_sequence (lexer : t) (seq : string) : t option =
  let len = String.length seq in
  let rec helper (lexer : t) (i : int) : t option =
    if i >= len then Some lexer
    else
      match peek_char lexer with
      | Some c when c = seq.[i] -> helper (advance_char lexer) (i + 1)
      | _ -> None
  in
  helper lexer 0

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
  | Some '>' -> make_token Token.RightBrace (advance_char lexer)
  | Some '=' -> make_token Token.PercentEqual (advance_char lexer)
  | Some ':' -> begin
      let lexer1 = advance_char lexer in
      match lex_sequence lexer1 "%:" with
      | Some lexer2 -> make_token Token.HashHash lexer2
      | _ -> make_token Token.Hash lexer1
    end
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
  | Some ':' -> make_token Token.LeftBracket (advance_char lexer)
  | Some '%' -> make_token Token.LeftBrace (advance_char lexer)
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

let lex_pp_number (lexer : t) (start_char : char) : Token.t * t =
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
  Buffer.add_char buf start_char;
  helper lexer buf

let lex_period (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '.' -> begin
      let next_lexer = advance_char lexer in
      match peek_char next_lexer with
      | Some '.' -> make_token Token.Ellipses (advance_char next_lexer)
      | _ -> make_token Token.Period lexer
    end
  | Some '0' .. '9' -> lex_pp_number lexer '.'
  | _ -> make_token Token.Period lexer

let lex_colon (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '>' -> make_token Token.RightBracket (advance_char lexer)
  | _ -> make_token Token.Colon lexer

let lex_hash (lexer : t) : Token.t * t =
  match peek_char lexer with
  | Some '#' -> make_token Token.HashHash (advance_char lexer)
  | _ -> make_token Token.Hash lexer

let lex_identifier (lexer : t) (start_char : char) : Token.t * t =
  let rec helper (lexer : t) (buf : Buffer.t) : Token.t * t =
    match peek_char lexer with
    | Some (('_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c) ->
        Buffer.add_char buf c;
        helper (advance_char lexer) buf
    | _ -> make_token (Token.Identifier (Buffer.contents buf)) lexer
  in

  let buf = Buffer.create 16 in
  Buffer.add_char buf start_char;
  helper lexer buf

let lex_char_literal (lexer : t) : Token.t * t =
  let rec helper (lexer : t) (sb : string_builder) : Token.t * t =
    match peek_char_view lexer with
    | Some { char = '\''; _ } -> begin
        let lexer = advance_char lexer in
        let kind =
          if Buffer.length sb.buffer = 0 then
            Token.Invalid Token.EmptyCharLiteral
          else
            Token.PPChar
              {
                string = Buffer.contents sb.buffer;
                positions = List.rev sb.positions;
              }
        in
        make_token kind lexer
      end
    | None | Some { char = '\n'; _ } -> begin
        let lexer = advance_char lexer in
        make_token (Token.Invalid Token.UnterminatedCharLiteral) lexer
      end
    | Some ({ char = '\\'; _ } as c) -> begin
        sb_add_char sb c;
        let next_lexer = advance_char lexer in

        match peek_char_view next_lexer with
        | Some c when c.char = '\\' || c.char = '\'' ->
            sb_add_char sb c;
            helper (advance_char next_lexer) sb
        | _ -> helper next_lexer sb
      end
    | Some c ->
        sb_add_char sb c;
        helper (advance_char lexer) sb
  in

  helper lexer (sb_create 2)

let lex_string_literal (lexer : t) : Token.t * t =
  let rec helper (lexer : t) (sb : string_builder) : Token.t * t =
    match peek_char_view lexer with
    | None | Some { char = '\n'; _ } -> begin
        let lexer = advance_char lexer in
        make_token (Token.Invalid Token.UnterminatedStringLiteral) lexer
      end
    | Some { char = '"'; _ } -> begin
        let lexer = advance_char lexer in
        let spliced_str : Source.string_src =
          {
            string = Buffer.contents sb.buffer;
            positions = List.rev sb.positions;
          }
        in
        make_token (Token.PPString spliced_str) lexer
      end
    | Some ({ char = '\\'; _ } as c) -> begin
        sb_add_char sb c;
        let next_lexer = advance_char lexer in

        match peek_char_view next_lexer with
        | Some c when c.char = '\\' || c.char = '"' ->
            sb_add_char sb c;
            helper (advance_char next_lexer) sb
        | _ -> helper next_lexer sb
      end
    | Some c ->
        sb_add_char sb c;
        helper (advance_char lexer) sb
  in
  helper lexer (sb_create 8)

let lex_token lexer =
  let lexer, tok = skip_whitespace lexer in
  match tok with
  | Some token -> (token, lexer)
  | None ->
      begin match peek_char_view lexer with
      | None -> make_token Token.Eof lexer
      | Some { char = c; pos; _ } ->
          let lexer = advance_char { lexer with start = pos } in
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
          | ',' -> make_token Token.Comma lexer
          | ';' -> make_token Token.Semicolon lexer
          | '?' -> make_token Token.Question lexer
          | ':' -> lex_colon lexer
          | '#' -> lex_hash lexer
          | '.' -> lex_period lexer
          (* Literals *)
          | '_' | 'a' .. 'z' | 'A' .. 'Z' -> lex_identifier lexer c
          | '0' .. '9' -> lex_pp_number lexer c
          | '\'' -> lex_char_literal lexer
          | '"' -> lex_string_literal lexer
          | _ -> make_token (Token.Invalid (Token.InvalidChar c)) lexer
          end
      end

let lex_header_name lexer =
  let rec continue_lexing (lexer : t) (buffer : Buffer.t)
      (type_ : Token.header_type) : Token.t * t =
    let finish_lexing (lexer : t) (buf : Buffer.t) (type_ : Token.header_type) =
      let filepath = Buffer.contents buf in
      make_token (Token.HeaderName { filepath; type_ }) (advance_char lexer)
    in
    match peek_char lexer with
    | Some '>' when type_ = Token.NonLocal ->
        finish_lexing lexer buffer Token.NonLocal
    | Some '"' when type_ = Token.Local ->
        finish_lexing lexer buffer Token.Local
    | Some '\n' | None ->
        make_token (Token.Invalid Token.UnterminatedHeaderName)
          (advance_char lexer)
    | Some c ->
        Buffer.add_char buffer c;
        continue_lexing (advance_char lexer) buffer type_
  in

  let lexer, tok = skip_whitespace lexer in
  match tok with
  | Some token -> (token, lexer)
  | None -> begin
      let lexer = { lexer with start = lexer.position } in
      match peek_char lexer with
      | Some '<' ->
          continue_lexing (advance_char lexer) (Buffer.create 16) Token.NonLocal
      | Some '"' ->
          continue_lexing (advance_char lexer) (Buffer.create 16) Token.Local
      | _ -> lex_token lexer
    end
