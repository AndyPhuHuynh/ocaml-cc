type defines = string list
type source = { id : Source.id; source : Source.t; lexer : Lexer.t }
type source_stack = { current : source; rest : source list; size : int }

type t = {
  diagnostics : Diagnostics.engine;
  defines : defines;
  source_manager : Source.manager;
  source_stack : source_stack;
}

let ( let* ) = Result.bind
let get_current_source (pp : t) : Source.t = pp.source_stack.current.source
let get_current_filepath (pp : t) : string = (get_current_source pp).filepath

let get_paths_from_include (current_file : string) (include_ : string) =
  Filename.concat (Filename.dirname current_file) include_

let lex_current (pp : t) : Token.t * Lexer.t =
  Lexer.lex_token pp.source_stack.current.lexer

let lex_current_header_name (pp : t) : Token.t * Lexer.t =
  Lexer.lex_header_name pp.source_stack.current.lexer

let update_lexer (pp : t) (lexer : Lexer.t) : t =
  let current = { pp.source_stack.current with lexer } in
  let source_stack = { pp.source_stack with current } in
  { pp with source_stack }

let append_source (pp : t) (source : Source.load_file)
    (include_location : Source.loc) : (t, Source.load_error) result =
  let* source_manager, id, source = Source.load_file pp.source_manager source in

  let current = { id; source; lexer = Lexer.create id source pp.diagnostics } in
  let source_stack =
    {
      current;
      rest = pp.source_stack.current :: pp.source_stack.rest;
      size = pp.source_stack.size + 1;
    }
  in
  Diagnostics.add_include pp.diagnostics
    pp.source_stack.current.source.display_name include_location;
  Ok { pp with source_manager; source_stack }

let pop_source (pp : t) : t =
  match pp.source_stack.rest with
  | [] -> failwith "Attempting to pop empty source"
  | x :: xs ->
      let source_stack =
        { current = x; rest = xs; size = pp.source_stack.size - 1 }
      in
      Diagnostics.remove_include pp.diagnostics;
      { pp with source_stack }

let rec skip_line (pp : t) =
  let token, lexer = lex_current pp in
  match token.kind with
  | Eof | NewLine -> update_lexer pp lexer
  | _ -> skip_line (update_lexer pp lexer)

let process_directive_include (pp : t) (include_location : Source.loc) : t =
  let token, lexer = lex_current_header_name pp in
  let pp = update_lexer pp lexer in
  match token.kind with
  | HeaderName { filepath = ""; _ } ->
      Diagnostics.emit_error pp.diagnostics
        (Diagnostics.at (get_current_source pp) token.loc "empty filename");
      skip_line pp
  | HeaderName { filepath; _ } -> begin
      let full_path =
        get_paths_from_include (get_current_filepath pp) filepath
      in

      if pp.source_stack.size >= 256 then begin
        Diagnostics.emit_fatal_error pp.diagnostics
          (Diagnostics.at (get_current_source pp) token.loc
             "maximum include depth exceeded")
          1
      end;

      match
        append_source pp
          { display_name = Some filepath; filepath = full_path }
          include_location
      with
      | Ok new_pp -> new_pp
      | Error (FileNotFound filepath) ->
          Diagnostics.emit_fatal_error pp.diagnostics
            (Diagnostics.from_span (get_current_source pp) token.span
               (Printf.sprintf "'%s' file not found" filepath))
            1
      | Error (IOError msg) ->
          Diagnostics.emit_fatal_error pp.diagnostics
            (Diagnostics.at (get_current_source pp) token.loc msg)
            1
    end
  | _ -> begin
      Diagnostics.emit_error pp.diagnostics
        (Diagnostics.at (get_current_source pp) token.loc
           "expected \"FILENAME\" or <FILENAME>");
      skip_line pp
    end

let rec process_directive_invalid (pp : t) : t =
  let invalid, lexer = lex_current pp in
  let msg =
    Printf.sprintf "invalid preprocessing directive: %s"
      (Source.span_to_string invalid.span pp.source_manager)
  in
  Diagnostics.emit_error pp.diagnostics
    (Diagnostics.at (get_current_source pp) invalid.loc msg);
  skip_line pp

let process_directive (pp : t) (hash_location : Source.loc) : t =
  let directive, lexer = lex_current pp in
  match directive.kind with
  | PPIdentifier { string = "include" } ->
      process_directive_include (update_lexer pp lexer) hash_location
  | NewLine -> update_lexer pp lexer
  | _ -> process_directive_invalid pp

let create (load_type : Source.load_file) (diagnostics : Diagnostics.engine) :
    (t, Source.load_error) result =
  let* new_manager, source_id, source =
    Source.load_file Source.empty_manager load_type
  in
  let lexer = Lexer.create source_id source diagnostics in

  Ok
    {
      diagnostics;
      defines = [];
      source_manager = new_manager;
      source_stack =
        { current = { id = source_id; source; lexer }; rest = []; size = 0 };
    }

let get_source_manager (pp : t) : Source.manager = pp.source_manager

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
      let pp = process_directive (update_lexer pp lexer) token.loc in
      next_token pp
  | _ -> (token, update_lexer pp lexer)
