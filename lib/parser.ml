let ( let* ) = Result.bind

type t = {
  diagnostics : Diagnostics.engine;
  converter : Token_converter.t;
  current_token : Token.t;
  next_token : Token.t;
}

type parse_error = t
type 'a parse_result = ('a, parse_error) result
type 'a parse_state_result = (t * 'a, parse_error) result

let create (load_file : Source.load_file) (diagnostics : Diagnostics.engine) :
    (t, Source.load_error) result =
  let* converter = Token_converter.create load_file diagnostics in
  let current_token, _, converter = Token_converter.next_token converter in
  let next_token, _, converter = Token_converter.next_token converter in
  Ok { diagnostics; converter; current_token; next_token }

let get_source_manager (parser : t) : Source.manager =
  Token_converter.get_source_manager parser.converter

let get_source (parser : t) (id : Source.id) : Source.t =
  Source.get_source (get_source_manager parser) id

let peek (parser : t) : Token.t = parser.current_token

let advance (parser : t) : t =
  let current_token = parser.next_token in
  let next_token, _, converter = Token_converter.next_token parser.converter in
  { parser with converter; current_token; next_token }

let expect (parser : t) (kind_tag : Token.kind_tag) (message : string) :
    Token.t parse_state_result =
  let token = peek parser in
  if Token.tag_of_kind token.kind <> kind_tag then begin
    Printf.printf "TODO: make this message better: %s\n" message;
    Error parser
  end
  else Ok (advance parser, token)

let expect_map (parser : t) (expect : Token.kind -> 'a option)
    (message : string) : 'a parse_state_result =
  let token = peek parser in
  match expect token.kind with
  | Some s -> Ok (advance parser, s)
  | None ->
      Printf.printf "TODO: make this message better: %s\n" message;
      Error parser

let expect_identifier (parser : t) (message : string) :
    string parse_state_result =
  expect_map parser
    (fun kind -> match kind with Identifier s -> Some s | _ -> None)
    message

let parse_declaration_specifiers (parser : t) : Syntax.declaration_specifiers =
  let rec helper (parser : t) (acc : Syntax.declaration_specifiers) :
      Syntax.declaration_specifiers =
    let token = peek parser in
    let next_parser = advance parser in
    match token.kind with
    (* storage class specifiers *)
    | Typedef -> helper next_parser (Syntax.add_storage_class acc Typedef token)
    | Extern -> helper next_parser (Syntax.add_storage_class acc Extern token)
    | Static -> helper next_parser (Syntax.add_storage_class acc Static token)
    | ThreadLocal ->
        helper next_parser (Syntax.add_storage_class acc ThreadLocal token)
    | Auto -> helper next_parser (Syntax.add_storage_class acc Auto token)
    | Register ->
        helper next_parser (Syntax.add_storage_class acc Register token)
    (* type specifiers *)
    | Void -> helper next_parser (Syntax.add_type_specifier acc Void token)
    | Char -> helper next_parser (Syntax.add_type_specifier acc Char token)
    | Short -> helper next_parser (Syntax.add_type_specifier acc Short token)
    | Int -> helper next_parser (Syntax.add_type_specifier acc Int token)
    | Long -> helper next_parser (Syntax.add_type_specifier acc Long token)
    | Float -> helper next_parser (Syntax.add_type_specifier acc Float token)
    | Double -> helper next_parser (Syntax.add_type_specifier acc Double token)
    | Signed -> helper next_parser (Syntax.add_type_specifier acc Signed token)
    | Unsigned ->
        helper next_parser (Syntax.add_type_specifier acc Unsigned token)
    (* type qualifiers *)
    | Const -> helper next_parser (Syntax.add_type_qualifier acc Const token)
    | Restrict ->
        helper next_parser (Syntax.add_type_qualifier acc Restrict token)
    | Volatile ->
        helper next_parser (Syntax.add_type_qualifier acc Volatile token)
    | _ -> acc
  in
  Syntax.reverse_specs (helper parser Syntax.empty_declaration_specifiers)

let analyze_object_storage_classes (parser : t)
    (specs : (Syntax.storage_class_specifier * Token.t) list) :
    Ast.object_storage =
  let module SpecSet = Set.Make (struct
    type t = Syntax.storage_class_specifier

    let compare = Stdlib.compare
  end) in
  let rec validate (first_spec : Syntax.storage_class_specifier option)
      (compatible_spec : Syntax.storage_class_specifier option)
      (encountered : SpecSet.t)
      (specs : (Syntax.storage_class_specifier * Token.t) list) :
      Syntax.storage_class_specifier option
      * Syntax.storage_class_specifier option =
    let check_duplicate (spec : Syntax.storage_class_specifier)
        (token : Token.t) : unit =
      if SpecSet.mem spec encountered then begin
        Diagnostics.emit_warning parser.diagnostics
          (Diagnostics.from_span
             (get_source parser token.span.source_id)
             token.span
             (Printf.sprintf
                "duplicate '%s' declaration specifier \
                 [-Wduplicate-decl-specifier]"
                (Syntax.string_of_storage_class_specifier spec)))
      end
    in

    let check_valid_combo (first_spec : Syntax.storage_class_specifier)
        (current_spec : Syntax.storage_class_specifier)
        (current_token : Token.t) : bool =
      if first_spec = current_spec then false
      else if
        (first_spec = ThreadLocal && current_spec = Static)
        || (first_spec = Static && current_spec = ThreadLocal)
        || (first_spec = ThreadLocal && current_spec = Extern)
        || (first_spec = Extern && current_spec = ThreadLocal)
      then true
      else begin
        Diagnostics.emit_error parser.diagnostics
          (Diagnostics.from_span
             (get_source parser current_token.span.source_id)
             current_token.span
             (Printf.sprintf
                "cannot combine '%s' with previous '%s' declaration specifier"
                (Syntax.string_of_storage_class_specifier current_spec)
                (Syntax.string_of_storage_class_specifier first_spec)));
        false
      end
    in

    match specs with
    | [] -> (first_spec, compatible_spec)
    | (spec, token) :: xs ->
        begin match first_spec with
        | None -> begin
            check_duplicate spec token;
            validate (Some spec) None (SpecSet.add spec encountered) xs
          end
        | Some first -> begin
            check_duplicate spec token;
            match check_valid_combo first spec token with
            | false ->
                validate first_spec compatible_spec
                  (SpecSet.add spec encountered)
                  xs
            | true ->
                validate (Some first) (Some spec)
                  (SpecSet.add spec encountered)
                  xs
          end
        end
  in
  match validate None None SpecSet.empty specs with
  | None, None -> NoStorage
  | None, Some _ ->
      failwith
        "internal error: analyze_object_storage_class has initial storage \
         class, but a compatible one was given"
  | Some spec, None ->
      begin match spec with
      | Typedef ->
          failwith
            "internal error: analyze_object_storage_class should not be called \
             with typedef as the first specifier"
      | Extern -> Extern
      | Static -> Static
      | ThreadLocal -> ThreadLocal
      | Auto -> Auto
      | Register -> Register
      end
  | Some ThreadLocal, Some spec | Some spec, Some ThreadLocal ->
      begin match spec with
      | Extern -> ThreadLocalExtern
      | Static -> ThreadLocalStatic
      | _ ->
          failwith
            (Printf.sprintf
               "internal error: invalid storage class specifier combo: '%s' \
                and '%s'"
               (Syntax.string_of_storage_class_specifier spec)
               (Syntax.string_of_storage_class_specifier ThreadLocal))
      end
  | Some first, Some second ->
      failwith
        (Printf.sprintf
           "internal error: invalid storage class specifier combo: '%s' and \
            '%s'"
           (Syntax.string_of_storage_class_specifier first)
           (Syntax.string_of_storage_class_specifier second))

let parse_declaration (parser : t) : Ast.declaration parse_state_result =
  let declaration_specifiers = parse_declaration_specifiers parser in
  let _ =
    analyze_object_storage_classes parser declaration_specifiers.storage_classes
  in

  (* let* parser, _ = expect parser Token.Int "int return type expected" in *)
  (* let* parser, name = *)
  (*   expect parser Token.Identifier "identifer name expected" *)
  (* in *)
  let ast : Ast.declaration =
    FunctionDeclaration { return_type = Int; name = "main" }
  in
  Ok (parser, ast)

let parse_translation_unit (parser : t) : unit =
  match parse_declaration parser with Ok _ -> () | Error _ -> ()
