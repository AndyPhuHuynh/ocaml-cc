type defines = string list
type source = { id : Source.id; source : Source.t; lexer : Lexer.t }
type source_stack = { current : source; rest : source list }

type t = {
  defines : defines;
  source_manager : Source.manager;
  source_stack : source_stack;
}

let make_absolute_path (path : string) =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let get_paths_from_include (current_file : string) (include_ : string) =
  let current_file = make_absolute_path current_file in
  Filename.concat (Filename.dirname current_file) include_

let read_entire_file (name : string) =
  In_channel.with_open_text name In_channel.input_all

let lex_current (engine : t) : Token.t * Lexer.t =
  Lexer.lex_token engine.source_stack.current.lexer

let lex_current_header_name (engine : t) : Token.t * Lexer.t =
  Lexer.lex_header_name engine.source_stack.current.lexer

let update_lexer (engine : t) (lexer : Lexer.t) : t =
  let current = { engine.source_stack.current with lexer } in
  let source_stack = { engine.source_stack with current } in
  { engine with source_stack }

let append_source (engine : t) (filepath : string) (contents : string) : t =
  let source = Source.create_source filepath contents in
  let source_manager, id = Source.add_source engine.source_manager source in

  let current = { id; source; lexer = Lexer.create id source } in
  let source_stack =
    { current; rest = engine.source_stack.current :: engine.source_stack.rest }
  in
  { engine with source_manager; source_stack }

let pop_source (engine : t) : t =
  match engine.source_stack.rest with
  | [] -> failwith "Attempting to pop empty source"
  | x :: xs ->
      let source_stack = { current = x; rest = xs } in
      { engine with source_stack }

let process_directive_include (engine : t) : t =
  let token, lexer = lex_current_header_name engine in
  let engine = update_lexer engine lexer in
  match token.kind with
  | Token.HeaderName { filepath; _ } -> begin
      let filepath =
        get_paths_from_include engine.source_stack.current.source.filepath
          filepath
      in
      let contents = read_entire_file filepath in
      append_source engine filepath contents
    end
  | _ -> begin
      Printf.printf "Expect headername, got: %s"
        (Token.to_string token engine.source_manager);
      exit 1
    end

let process_directive (engine : t) : t =
  let directive, lexer = lex_current engine in
  match directive.kind with
  | Identifier "include" ->
      process_directive_include (update_lexer engine lexer)
  | Identifier str ->
      Printf.printf "TODO: directive %s" str;
      exit 1
  | _ ->
      print_endline "TODO: Non directive found after hash";
      exit 1

let init filepath =
  let filepath = make_absolute_path filepath in
  let contents = read_entire_file filepath in

  let source_manager = Source.create_manager in
  let source : Source.t = { filepath; contents } in
  let new_manager, source_id = Source.add_source source_manager source in
  let lexer = Lexer.create source_id source in

  {
    defines = [];
    source_manager = new_manager;
    source_stack = { current = { id = source_id; source; lexer }; rest = [] };
  }

let rec next_token (engine : t) : Token.t * t =
  let token, lexer = lex_current engine in
  match token.kind with
  | Token.Eof ->
      begin match engine.source_stack.rest with
      | [] -> (token, engine)
      | _ -> next_token (pop_source engine)
      end
  | Token.NewLine -> next_token (update_lexer engine lexer)
  | Token.Hash when token.is_at_line_start ->
      let engine = process_directive (update_lexer engine lexer) in
      next_token engine
  | _ -> (token, update_lexer engine lexer)

let tokenize_all (filepath : string) : Token.t list * Source.manager =
  let rec helper (engine : t) (acc : Token.t list) :
      Token.t list * Source.manager =
    let tok, engine = next_token engine in
    match tok.kind with
    | Token.Eof -> (List.rev (tok :: acc), engine.source_manager)
    | _ -> helper engine (tok :: acc)
  in
  let engine = init filepath in
  helper engine []
