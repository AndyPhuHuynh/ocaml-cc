type defines = string list
type location = { line : int; col : int }

type source = {
  id : Source.id;
  source : Source.t;
  lexer : Lexer.t;
  child_include_location : location option;
}

type source_stack = { current : source; rest : source list; size : int }

type t = {
  defines : defines;
  include_stack_printed : bool;
  source_manager : Source.manager;
  source_stack : source_stack;
}

let ( let* ) = Result.bind
let get_current_source (pp : t) : Source.t = pp.source_stack.current.source
let get_current_filepath (pp : t) : string = (get_current_source pp).filepath

let print_include_stack (pp : t) : unit =
  let stack = List.rev pp.source_stack.rest in
  List.iter
    (fun source ->
      let { line; col } = Option.get source.child_include_location in
      Printf.eprintf "In file included from %s:%d:%d\n" source.source.filepath
        line col)
    stack

let emit_error (pp : t) (line : int) (col : int) (msg : string) : t =
  let new_pp =
    if not pp.include_stack_printed then begin
      print_include_stack pp;
      Some { pp with include_stack_printed = true }
    end
    else None
  in
  Diagnostics.emit_error (get_current_filepath pp) line col msg;
  Option.value new_pp ~default:pp

let emit_fatal_error (pp : t) (line : int) (col : int) (msg : string)
    (exit_code : int) : 'a =
  if not pp.include_stack_printed then begin
    print_include_stack pp
  end;
  Diagnostics.emit_fatal_error (get_current_filepath pp) line col msg exit_code

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

let append_source (pp : t) (filepath : string) (include_location : location) :
    (t, Source.load_error) result =
  let* source_manager, id, source =
    Source.load_file pp.source_manager filepath
  in

  let current =
    {
      id;
      source;
      lexer = Lexer.create id source;
      child_include_location = None;
    }
  in
  let source_stack =
    {
      current;
      rest =
        {
          pp.source_stack.current with
          child_include_location = Some include_location;
        }
        :: pp.source_stack.rest;
      size = pp.source_stack.size + 1;
    }
  in
  Ok { pp with include_stack_printed = false; source_manager; source_stack }

let pop_source (pp : t) : t =
  match pp.source_stack.rest with
  | [] -> failwith "Attempting to pop empty source"
  | x :: xs ->
      let source_stack =
        {
          current = { x with child_include_location = None };
          rest = xs;
          size = pp.source_stack.size - 1;
        }
      in
      { pp with include_stack_printed = false; source_stack }

let rec skip_line (pp : t) =
  let token, lexer = lex_current pp in
  match token.kind with
  | Eof | NewLine -> update_lexer pp lexer
  | _ -> skip_line (update_lexer pp lexer)

let process_directive_include (pp : t) (include_location : location) : t =
  let token, lexer = lex_current_header_name pp in
  let pp = update_lexer pp lexer in
  match token.kind with
  | HeaderName { filepath = ""; _ } ->
      let pp =
        emit_error pp token.line token.col
          "empty filename in #include directive"
      in
      skip_line pp
  | HeaderName { filepath; _ } -> begin
      let filepath =
        get_paths_from_include (get_current_filepath pp) filepath
      in

      if pp.source_stack.size >= 256 then begin
        Diagnostics.emit_fatal_error (get_current_filepath pp) token.line
          token.col "maximum include depth exceeded" 1
      end;

      match append_source pp filepath include_location with
      | Ok new_pp -> new_pp
      | Error FileNotFound ->
          Diagnostics.emit_fatal_error (get_current_filepath pp) token.line
            token.col
            (Printf.sprintf "'%s' file not found" filepath)
            1
      | Error (IOError msg) ->
          Diagnostics.emit_fatal_error (get_current_filepath pp) token.line
            token.col msg 1
    end
  | _ -> begin
      Printf.printf "Expect headername, got: %s"
        (Token.to_string token pp.source_manager);
      exit 1
    end

let rec process_directive_invalid (pp : t) : t =
  let invalid, lexer = lex_current pp in
  let msg =
    Printf.sprintf "invalid preprocessing directive: %s"
      (Source.span_to_string invalid.span pp.source_manager)
  in
  Diagnostics.emit_error (get_current_filepath pp) invalid.line invalid.col msg;
  skip_line pp

let process_directive (pp : t) (hash_location : location) : t =
  let directive, lexer = lex_current pp in
  match directive.kind with
  | Identifier "include" ->
      process_directive_include (update_lexer pp lexer) hash_location
  | NewLine -> update_lexer pp lexer
  | _ -> process_directive_invalid pp

let create (filepath : string) : (t, Source.load_error) result =
  let source_manager = Source.empty_manager in
  let* new_manager, source_id, source =
    Source.load_file source_manager filepath
  in
  let lexer = Lexer.create source_id source in

  Ok
    {
      defines = [];
      include_stack_printed = false;
      source_manager = new_manager;
      source_stack =
        {
          current =
            { id = source_id; source; lexer; child_include_location = None };
          rest = [];
          size = 0;
        };
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
      let pp =
        process_directive (update_lexer pp lexer)
          { line = token.line; col = token.col }
      in
      next_token pp
  | _ -> (token, update_lexer pp lexer)

let tokenize_all (filepath : string) :
    (Token.t list * Source.manager, Source.load_error) result =
  let rec helper (pp : t) (acc : Token.t list) : Token.t list * Source.manager =
    let tok, pp = next_token pp in
    match tok.kind with
    | Eof -> (List.rev (tok :: acc), pp.source_manager)
    | _ -> helper pp (tok :: acc)
  in
  let* pp = create filepath in
  Ok (helper pp [])
