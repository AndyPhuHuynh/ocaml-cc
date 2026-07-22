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

let lex_current (pp : t) : Token.t * Lexer.t =
  Lexer.lex_token pp.source_stack.current.lexer

let lex_current_header_name (pp : t) : Token.t * Lexer.t =
  Lexer.lex_header_name pp.source_stack.current.lexer

let update_lexer (pp : t) (lexer : Lexer.t) : t =
  let current = { pp.source_stack.current with lexer } in
  let source_stack = { pp.source_stack with current } in
  { pp with source_stack }

let append_source (pp : t) (filepath : string) (contents : string) : t =
  let source = Source.create_source filepath contents in
  let source_manager, id = Source.add_source pp.source_manager source in

  let current = { id; source; lexer = Lexer.create id source } in
  let source_stack =
    { current; rest = pp.source_stack.current :: pp.source_stack.rest }
  in
  { pp with source_manager; source_stack }

let pop_source (pp : t) : t =
  match pp.source_stack.rest with
  | [] -> failwith "Attempting to pop empty source"
  | x :: xs ->
      let source_stack = { current = x; rest = xs } in
      { pp with source_stack }

let process_directive_include (pp : t) : t =
  let token, lexer = lex_current_header_name pp in
  let pp = update_lexer pp lexer in
  match token.kind with
  | Token.HeaderName { filepath; _ } -> begin
      let filepath =
        get_paths_from_include pp.source_stack.current.source.filepath filepath
      in
      let contents = read_entire_file filepath in
      append_source pp filepath contents
    end
  | _ -> begin
      Printf.printf "Expect headername, got: %s"
        (Token.to_string token pp.source_manager);
      exit 1
    end

let process_directive (pp : t) : t =
  let directive, lexer = lex_current pp in
  match directive.kind with
  | Identifier "include" -> process_directive_include (update_lexer pp lexer)
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

let rec next_token (pp : t) : Token.t * t =
  let token, lexer = lex_current pp in
  match token.kind with
  | Token.Eof ->
      begin match pp.source_stack.rest with
      | [] -> (token, pp)
      | _ -> next_token (pop_source pp)
      end
  | Token.NewLine -> next_token (update_lexer pp lexer)
  | Token.Hash when token.is_at_line_start ->
      let pp = process_directive (update_lexer pp lexer) in
      next_token pp
  | _ -> (token, update_lexer pp lexer)

let tokenize_all (filepath : string) : Token.t list * Source.manager =
  let rec helper (pp : t) (acc : Token.t list) : Token.t list * Source.manager =
    let tok, pp = next_token pp in
    match tok.kind with
    | Token.Eof -> (List.rev (tok :: acc), pp.source_manager)
    | _ -> helper pp (tok :: acc)
  in
  let pp = init filepath in
  helper pp []
