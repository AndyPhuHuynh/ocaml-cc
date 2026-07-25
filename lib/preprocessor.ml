type defines = string list
type source = { id : Source.id; source : Source.t; lexer : Lexer.t }
type source_stack = { current : source; rest : source list; size : int }

type t = {
  defines : defines;
  source_manager : Source.manager;
  source_stack : source_stack;
}

let get_current_source (pp : t) : Source.t = pp.source_stack.current.source
let get_current_filepath (pp : t) : string = (get_current_source pp).filepath

let get_paths_from_include (current_file : string) (include_ : string) =
  let current_file = Source.make_absolute_path current_file in
  Filename.concat (Filename.dirname current_file) include_

let lex_current (pp : t) : Token.t * Lexer.t =
  Lexer.lex_token pp.source_stack.current.lexer

let lex_current_header_name (pp : t) : Token.t * Lexer.t =
  Lexer.lex_header_name pp.source_stack.current.lexer

let update_lexer (pp : t) (lexer : Lexer.t) : t =
  let current = { pp.source_stack.current with lexer } in
  let source_stack = { pp.source_stack with current } in
  { pp with source_stack }

let append_source (pp : t) (filepath : string) : t =
  let source_manager, id, source =
    Source.load_file pp.source_manager filepath
  in

  let current = { id; source; lexer = Lexer.create id source } in
  let source_stack =
    {
      current;
      rest = pp.source_stack.current :: pp.source_stack.rest;
      size = pp.source_stack.size + 1;
    }
  in
  { pp with source_manager; source_stack }

let pop_source (pp : t) : t =
  match pp.source_stack.rest with
  | [] -> failwith "Attempting to pop empty source"
  | x :: xs ->
      let source_stack =
        { current = x; rest = xs; size = pp.source_stack.size - 1 }
      in
      { pp with source_stack }

let process_directive_include (pp : t) : t =
  let token, lexer = lex_current_header_name pp in
  let pp = update_lexer pp lexer in
  match token.kind with
  | HeaderName { filepath; _ } -> begin
      let filepath =
        get_paths_from_include (get_current_filepath pp) filepath
      in

      try
        if not (Source.is_regular_file filepath) then begin
          Diagnostics.emit_fatal_error (get_current_filepath pp) token.line
            token.col
            (Printf.sprintf "'%s' file not found" filepath)
            1
        end;
        let new_pp = append_source pp filepath in
        if new_pp.source_stack.size >= 256 then begin
          Diagnostics.emit_fatal_error (get_current_filepath pp) token.line
            token.col "maximum include depth exceeded" 1
        end;
        new_pp
      with Sys_error error_msg ->
        Diagnostics.emit_fatal_error (get_current_filepath pp) token.line
          token.col error_msg 1
    end
  | _ -> begin
      Printf.printf "Expect headername, got: %s"
        (Token.to_string token pp.source_manager);
      exit 1
    end

let rec process_directive_invalid (pp : t) : t =
  let rec skip_line (pp : t) =
    let token, lexer = lex_current pp in
    match token.kind with
    | Eof | NewLine -> update_lexer pp lexer
    | _ -> skip_line (update_lexer pp lexer)
  in

  let invalid, lexer = lex_current pp in
  let msg =
    Printf.sprintf "invalid preprocessing directive: %s"
      (Source.span_to_string invalid.span pp.source_manager)
  in
  Diagnostics.emit_error (get_current_filepath pp) invalid.line invalid.col msg;
  skip_line pp

let process_directive (pp : t) : t =
  let directive, lexer = lex_current pp in
  match directive.kind with
  | Identifier "include" -> process_directive_include (update_lexer pp lexer)
  | NewLine -> update_lexer pp lexer
  | _ -> process_directive_invalid pp

let init filepath =
  let filepath = Source.make_absolute_path filepath in

  let source_manager = Source.empty_manager in
  let new_manager, source_id, source =
    Source.load_file source_manager filepath
  in
  let lexer = Lexer.create source_id source in

  {
    defines = [];
    source_manager = new_manager;
    source_stack =
      { current = { id = source_id; source; lexer }; rest = []; size = 0 };
  }

let rec next_token (pp : t) : Token.t * t =
  let token, lexer = lex_current pp in
  match token.kind with
  | Eof ->
      begin match pp.source_stack.rest with
      | [] -> (token, pp)
      | _ -> next_token (pop_source pp)
      end
  | NewLine -> next_token (update_lexer pp lexer)
  | Hash when token.is_at_line_start ->
      let pp = process_directive (update_lexer pp lexer) in
      next_token pp
  | _ -> (token, update_lexer pp lexer)

let tokenize_all (filepath : string) : Token.t list * Source.manager =
  let rec helper (pp : t) (acc : Token.t list) : Token.t list * Source.manager =
    let tok, pp = next_token pp in
    match tok.kind with
    | Eof -> (List.rev (tok :: acc), pp.source_manager)
    | _ -> helper pp (tok :: acc)
  in
  let pp = init filepath in
  helper pp []
