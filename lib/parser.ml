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

(* ------------------ *)
(* Diagnostic helpers *)
(* ------------------ *)

let diagnostics_emit_warning (parser : t) (diag : Diagnostics.t) =
  Diagnostics.emit_warning parser.diagnostics diag

let diagnostics_emit_error (parser : t) (diag : Diagnostics.t) =
  Diagnostics.emit_error parser.diagnostics diag

let diagnostics_from_token (parser : t) (token : Token.t) (message : string) :
    Diagnostics.t =
  Diagnostics.from_span
    (get_source parser token.span.source_id)
    token.span message
(* ------------------ *)
(* Parse declarations *)
(* ------------------ *)

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

let analyze_storage_classes (parser : t)
    (specs : (Syntax.storage_class_specifier * Token.t) list) :
    (Syntax.storage_class_specifier * Token.t) option
    * (Syntax.storage_class_specifier * Token.t) option =
  let module SpecSet = Set.Make (struct
    type t = Syntax.storage_class_specifier

    let compare = Stdlib.compare
  end) in
  let rec validate
      (first_spec : (Syntax.storage_class_specifier * Token.t) option)
      (compatible_spec : (Syntax.storage_class_specifier * Token.t) option)
      (encountered : SpecSet.t)
      (specs : (Syntax.storage_class_specifier * Token.t) list) :
      (Syntax.storage_class_specifier * Token.t) option
      * (Syntax.storage_class_specifier * Token.t) option =
    let check_duplicate (spec : Syntax.storage_class_specifier)
        (token : Token.t) : unit =
      if SpecSet.mem spec encountered then begin
        diagnostics_emit_warning parser
          (diagnostics_from_token parser token
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
        diagnostics_emit_error parser
          (diagnostics_from_token parser current_token
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
            validate (Some (spec, token)) None (SpecSet.add spec encountered) xs
          end
        | Some (first_spec, first_token) -> begin
            check_duplicate spec token;
            match check_valid_combo first_spec spec token with
            | false ->
                validate
                  (Some (first_spec, first_token))
                  compatible_spec
                  (SpecSet.add spec encountered)
                  xs
            | true ->
                validate
                  (Some (first_spec, first_token))
                  (Some (spec, token))
                  (SpecSet.add spec encountered)
                  xs
          end
        end
  in
  validate None None SpecSet.empty specs

let analyze_object_storage_classes (parser : t)
    (specs : (Syntax.storage_class_specifier * Token.t) list) :
    Ast.object_storage =
  match analyze_storage_classes parser specs with
  | None, None -> NoStorage
  | None, Some _ ->
      failwith
        "internal error: analyze_object_storage_class has no initial storage \
         class, but a compatible one was given"
  | Some (spec, _), None ->
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
  | Some (ThreadLocal, _), Some (spec, token)
  | Some (spec, token), Some (ThreadLocal, _) ->
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
           (Syntax.string_of_storage_class_specifier (fst first))
           (Syntax.string_of_storage_class_specifier (fst second)))

let analyze_function_storage_classes (parser : t)
    (specs : (Syntax.storage_class_specifier * Token.t) list) :
    Ast.function_storage =
  let emit_storage_class_error (spec : Syntax.storage_class_specifier)
      (token : Token.t) : unit =
    diagnostics_emit_error parser
      (diagnostics_from_token parser token
         (Printf.sprintf
            "storage class specifier '%s' is not allowed on a function; must \
             be extern or static"
            (Syntax.string_of_storage_class_specifier spec)))
  in

  match analyze_storage_classes parser specs with
  | None, None -> NoStorage
  | None, Some _ ->
      failwith
        "internal error: analyze_function_storage_class has no initial storage \
         class, but a compatible one was given"
  | Some (Extern, _), None -> Extern
  | Some (Static, _), None -> Static
  | Some (ThreadLocal, _), Some (spec, token)
  | Some (spec, token), Some (ThreadLocal, _) ->
      begin match spec with
      | Extern ->
          emit_storage_class_error ThreadLocal token;
          Extern
      | Static ->
          emit_storage_class_error ThreadLocal token;
          Static
      | _ ->
          failwith
            (Printf.sprintf
               "internal error: invalid storage class specifier combo: '%s' \
                and '%s'"
               (Syntax.string_of_storage_class_specifier spec)
               (Syntax.string_of_storage_class_specifier ThreadLocal))
      end
  | Some (spec, token), None ->
      begin match spec with
      | Typedef ->
          failwith
            "internal error: analyze_function_storage_class should not be \
             called with typedef as the first specifier"
      | _ ->
          emit_storage_class_error spec token;
          NoStorage
      end
  | Some first, Some second ->
      failwith
        (Printf.sprintf
           "internal error: invalid storage class specifier combo: '%s' and \
            '%s'"
           (Syntax.string_of_storage_class_specifier (fst first))
           (Syntax.string_of_storage_class_specifier (fst second)))

let analyze_typedef_storage_classes (parser : t)
    (specs : (Syntax.storage_class_specifier * Token.t) list) : unit =
  let get_spec_string (spec : (Syntax.storage_class_specifier * Token.t) option)
      : string =
    match spec with
    | None -> "None"
    | Some x -> Syntax.string_of_storage_class_specifier (fst x)
  in

  match analyze_storage_classes parser specs with
  | Some (Typedef, _), None -> ()
  | None, Some _ ->
      failwith
        "internal error: analyze_typedef_storage_class has no initial storage \
         class, but a compatible one was given"
  | spec1, spec2 ->
      failwith
        (Printf.sprintf
           "internal error: analyze_typedef_storage_class invalid storage \
            class specifier configuration: (%s, %s)"
           (get_spec_string spec1) (get_spec_string spec2))

let analyze_type_qualifiers (parser : t)
    (specs : (Syntax.type_qualifier * Token.t) list) : Ast.type_qualifiers =
  let warn_duplicate (spec : Syntax.type_qualifier) (token : Token.t) : unit =
    diagnostics_emit_warning parser
      (diagnostics_from_token parser token
         (Printf.sprintf
            "duplicate '%s' declaration specifier [-Wduplicate-decl-specifier]"
            (Syntax.string_of_type_qualifier spec)))
  in

  let rec helper (specs : (Syntax.type_qualifier * Token.t) list)
      (acc : Ast.type_qualifiers) : Ast.type_qualifiers =
    match specs with
    | [] -> acc
    | (spec, token) :: xs ->
        begin match spec with
        | Const ->
            begin if acc.const then begin
              warn_duplicate spec token;
              helper xs acc
            end
            else begin
              helper xs { acc with const = true }
            end
            end
        | Restrict ->
            begin if acc.restrict then begin
              warn_duplicate spec token;
              helper xs acc
            end
            else begin
              helper xs { acc with restrict = true }
            end
            end
        | Volatile ->
            begin if acc.volatile then begin
              warn_duplicate spec token;
              helper xs acc
            end
            else begin
              helper xs { acc with volatile = true }
            end
            end
        end
  in
  helper specs { const = false; restrict = false; volatile = false }

let parse_declaration (parser : t) : Ast.declaration parse_state_result =
  let declaration_specifiers = parse_declaration_specifiers parser in
  let _ =
    analyze_function_storage_classes parser
      declaration_specifiers.storage_classes
  in
  let _ =
    analyze_type_qualifiers parser declaration_specifiers.type_qualifiers
  in

  (* let* parser, _ = expect parser Token.Int "int return type expected" in *)
  (* let* parser, name = *)
  (*   expect parser Token.Identifier "identifer name expected" *)
  (* in *)
  let ast : Ast.declaration =
    FunctionDeclaration
      {
        return_type =
          {
            qualifiers = { const = false; restrict = false; volatile = false };
            kind = Int;
          };
        name = "main";
      }
  in
  Ok (parser, ast)

let parse_translation_unit (parser : t) : unit =
  match parse_declaration parser with Ok _ -> () | Error _ -> ()
