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
type parse_declarator_error = NormalError | NoIdentifier

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
let peek_next (parser : t) : Token.t = parser.next_token

let advance (parser : t) : t =
  let current_token = parser.next_token in
  let next_token, _, converter = Token_converter.next_token parser.converter in
  { parser with converter; current_token; next_token }

let peek_and_advance (parser : t) : t * Token.t =
  let curr_token = peek parser in
  (advance parser, curr_token)

(* ------------------- *)
(* --- Diagnostics --- *)
(* ------------------- *)

(* ------------------------------------ *)
(* --- Diagnostic --- Emit Wrappers --- *)
(* ------------------------------------ *)

let diagnostics_emit_warning (parser : t) (diag : Diagnostics.t) =
  Diagnostics.emit_warning parser.diagnostics diag

let diagnostics_emit_error (parser : t) (diag : Diagnostics.t) =
  Diagnostics.emit_error parser.diagnostics diag

(* ------------------------------------------- *)
(* --- Diagnostic --- Constructor Wrappers --- *)
(* ------------------------------------------- *)

let diagnostics_at_token_loc (parser : t) (token : Token.t) (message : string) :
    Diagnostics.t =
  Diagnostics.at
    (get_source parser token.info.span.source_id)
    token.info.loc message

let diagnostics_from_token_span (parser : t) (token : Token.t)
    (message : string) : Diagnostics.t =
  Diagnostics.from_span
    (get_source parser token.info.span.source_id)
    token.info.span message

(* ----------------------------------------- *)
(* --- Diagnostic --- Construct and Emit --- *)
(* ----------------------------------------- *)

let emit_warning_token_loc (parser : t) (token : Token.t) (message : string) :
    unit =
  diagnostics_emit_warning parser
    (diagnostics_at_token_loc parser token message)

let emit_warning_token_span (parser : t) (token : Token.t) (message : string) :
    unit =
  diagnostics_emit_warning parser
    (diagnostics_from_token_span parser token message)

let emit_error_token_loc (parser : t) (token : Token.t) (message : string) :
    unit =
  diagnostics_emit_error parser (diagnostics_at_token_loc parser token message)

let emit_error_token_span (parser : t) (token : Token.t) (message : string) :
    unit =
  diagnostics_emit_error parser
    (diagnostics_from_token_span parser token message)

(* ------ *)
(* Expect *)
(* ------ *)

let expect (parser : t) (kind_tag : Token.kind_tag) (message : string) :
    Token.t parse_state_result =
  let token = peek parser in
  if Token.tag_of_kind token.kind <> kind_tag then begin
    emit_error_token_span parser token message;
    Error parser
  end
  else Ok (advance parser, token)

let expect_map (parser : t) (expect : Token.t -> 'a option) (message : string) :
    'a parse_state_result =
  let token = peek parser in
  match expect token with
  | Some s -> Ok (advance parser, s)
  | None ->
      emit_error_token_span parser token message;
      Error parser

let expect_identifier (parser : t) (message : string) :
    (string * Token.info) parse_state_result =
  expect_map parser
    (fun token ->
      match token.kind with Identifier s -> Some (s, token.info) | _ -> None)
    message

let expect_int_literal (parser : t) (message : string) :
    (Token.int_literal * Token.info) parse_state_result =
  expect_map parser
    (fun token ->
      match token.kind with IntLiteral s -> Some (s, token.info) | _ -> None)
    message

(* ----------------------- *)
(* --- Type Specifiers --- *)
(* ----------------------- *)

(* TODO: enum, typedef-name *)
let rec is_token_type_specifier (kind : Token.kind) : bool =
  match kind with
  | Void | Char | Short | Int | Long | Float | Double | Signed | Unsigned | Bool
  | Complex | Atomic | Struct | Union ->
      true
  | _ -> false

and parse_type_specifier (parser : t) : Syntax.type_specifier parse_state_result
    =
  let next_parser, next_token = peek_and_advance parser in
  match next_token.kind with
  | Void -> Ok (next_parser, Void)
  | Char -> Ok (next_parser, Char)
  | Short -> Ok (next_parser, Short)
  | Int -> Ok (next_parser, Int)
  | Long -> Ok (next_parser, Long)
  | Float -> Ok (next_parser, Float)
  | Double -> Ok (next_parser, Double)
  | Signed -> Ok (next_parser, Signed)
  | Unsigned -> Ok (next_parser, Unsigned)
  | Bool -> Ok (next_parser, Bool)
  | Complex -> Ok (next_parser, Complex)
  | Atomic -> begin
      let* parser, _ = expect next_parser LeftParen "expect '('" in
      let* parser, type_name = parse_type_name parser in
      let* parser, _ = expect parser RightParen "expect ')'" in
      Ok (parser, Syntax.Atomic type_name)
    end
  | _ ->
      emit_warning_token_loc parser next_token "expected type specifier";
      Error parser

(* --------------------------- *)
(* --- Type Qualifier List --- *)
(* --------------------------- *)

and analyze_type_qualifiers (parser : t)
    (specs : (Syntax.type_qualifier * Token.t) list) : Syntax.type_qualifiers =
  let warn_duplicate (spec : Syntax.type_qualifier) (token : Token.t) : unit =
    emit_warning_token_span parser token
      (Printf.sprintf
         "duplicate '%s' declaration specifier [-Wduplicate-decl-specifier]"
         (Syntax.string_of_type_qualifier spec))
  in

  let rec helper (specs : (Syntax.type_qualifier * Token.t) list)
      (acc : Syntax.type_qualifiers) : Syntax.type_qualifiers =
    match specs with
    | [] -> acc
    | (spec, token) :: xs ->
        begin match spec with
        | Const -> begin
            if acc.const then warn_duplicate spec token;
            helper xs { acc with const = true }
          end
        | Restrict -> begin
            if acc.restrict then warn_duplicate spec token;
            helper xs { acc with restrict = true }
          end
        | Volatile -> begin
            if acc.volatile then warn_duplicate spec token;
            helper xs { acc with volatile = true }
          end
        end
  in
  helper specs Syntax.empty_type_qualifiers

and parse_type_qualifier_list (parser : t) : t * Syntax.type_qualifiers =
  let rec helper (parser : t) (acc : (Syntax.type_qualifier * Token.t) list) :
      t * Syntax.type_qualifiers =
    let token = peek parser in
    let next_parser = advance parser in
    match token.kind with
    | Const -> helper next_parser ((Const, token) :: acc)
    | Restrict -> helper next_parser ((Restrict, token) :: acc)
    | Volatile -> helper next_parser ((Volatile, token) :: acc)
    | _ -> (parser, analyze_type_qualifiers parser (List.rev acc))
  in
  helper parser []

(* ---------------- *)
(* --- Pointers --- *)
(* ---------------- *)

and parse_pointer (parser : t) : Syntax.type_qualifiers parse_state_result =
  let* parser, _ = expect parser Token.Star "expected pointer" in
  let parser, qualifiers = parse_type_qualifier_list parser in
  Ok (parser, qualifiers)

and parse_pointer_list (parser : t) :
    Syntax.type_qualifiers list parse_state_result =
  let rec helper (parser : t) (acc : Syntax.type_qualifiers list) :
      Syntax.type_qualifiers list parse_state_result =
    let token = peek parser in
    match token.kind with
    | Star -> begin
        let* parser, qualifiers = parse_pointer parser in
        helper parser (qualifiers :: acc)
      end
    | _ -> Ok (parser, acc)
  in
  helper parser []

(* -------------------------------- *)
(* --- Specifier Qualifier List --- *)
(* -------------------------------- *)

and parse_specifier_qualifier_list (parser : t) :
    Syntax.specifier_qualifier_list parse_state_result =
  let rec helper (parser : t) (acc : Syntax.specifier_qualifier_list) :
      Syntax.specifier_qualifier_list parse_state_result =
    let token = peek parser in
    let next_parser = advance parser in
    match token.kind with
    (* type specifiers *)
    | _ when is_token_type_specifier token.kind -> begin
        let* parser, type_specifier = parse_type_specifier parser in
        helper parser (Syntax.add_sq_type_specifier acc type_specifier token)
      end
    (* type qualifiers *)
    | Const -> helper next_parser (Syntax.add_sq_type_qualifier acc Const token)
    | Restrict ->
        helper next_parser (Syntax.add_sq_type_qualifier acc Restrict token)
    | Volatile ->
        helper next_parser (Syntax.add_sq_type_qualifier acc Volatile token)
    | _ -> Ok (parser, acc)
  in

  let* parser, specs = helper parser Syntax.empty_specifier_qualifier_list in
  Ok (parser, Syntax.reverse_specifier_qualifier_list specs)

(* ------------------------------ *)
(* --- Declaration Specifiers --- *)
(* ------------------------------ *)

and parse_declaration_specifiers (parser : t) :
    Syntax.declaration_specifiers parse_state_result =
  let rec helper (parser : t) (acc : Syntax.declaration_specifiers) :
      Syntax.declaration_specifiers parse_state_result =
    let token = peek parser in
    let next_parser = advance parser in
    match token.kind with
    (* storage class specifiers *)
    | Typedef ->
        helper next_parser (Syntax.add_decl_storage_class acc Typedef token)
    | Extern ->
        helper next_parser (Syntax.add_decl_storage_class acc Extern token)
    | Static ->
        helper next_parser (Syntax.add_decl_storage_class acc Static token)
    | ThreadLocal ->
        helper next_parser (Syntax.add_decl_storage_class acc ThreadLocal token)
    | Auto -> helper next_parser (Syntax.add_decl_storage_class acc Auto token)
    | Register ->
        helper next_parser (Syntax.add_decl_storage_class acc Register token)
    (* type specifiers *)
    | _ when is_token_type_specifier token.kind -> begin
        let* parser, type_specifier = parse_type_specifier parser in
        helper parser (Syntax.add_decl_type_specifier acc type_specifier token)
      end
    (* type qualifiers *)
    | Const ->
        helper next_parser (Syntax.add_decl_type_qualifier acc Const token)
    | Restrict ->
        helper next_parser (Syntax.add_decl_type_qualifier acc Restrict token)
    | Volatile ->
        helper next_parser (Syntax.add_decl_type_qualifier acc Volatile token)
    | _ -> Ok (parser, acc)
  in

  let* parser, specs = helper parser Syntax.empty_declaration_specifiers in
  Ok (parser, Syntax.reverse_declaration_specifiers specs)

and analyze_storage_classes (parser : t)
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
        emit_warning_token_span parser token
          (Printf.sprintf
             "duplicate '%s' declaration specifier [-Wduplicate-decl-specifier]"
             (Syntax.string_of_storage_class_specifier spec))
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
        emit_error_token_span parser current_token
          (Printf.sprintf
             "cannot combine '%s' with previous '%s' declaration specifier"
             (Syntax.string_of_storage_class_specifier current_spec)
             (Syntax.string_of_storage_class_specifier first_spec));
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

and analyze_object_storage_classes (parser : t)
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

and analyze_function_storage_classes (parser : t)
    (specs : (Syntax.storage_class_specifier * Token.t) list) :
    Ast.function_storage =
  let emit_storage_class_error (spec : Syntax.storage_class_specifier)
      (token : Token.t) : unit =
    emit_error_token_span parser token
      (Printf.sprintf
         "storage class specifier '%s' is not allowed on a function; must be \
          extern or static"
         (Syntax.string_of_storage_class_specifier spec))
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

and analyze_typedef_storage_classes (parser : t)
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

and analyze_function_specifiers (parser : t)
    (specs : (Syntax.function_specifier * Token.t) list) :
    Ast.function_specifiers =
  let warn_duplicate (spec : Syntax.function_specifier) (token : Token.t) : unit
      =
    emit_warning_token_span parser token
      (Printf.sprintf
         "duplicate '%s' declaration specifier [-Wduplicate-decl-specifier]"
         (Syntax.string_of_function_specifier spec))
  in

  let rec helper (specs : (Syntax.function_specifier * Token.t) list)
      (acc : Ast.function_specifiers) : Ast.function_specifiers =
    match specs with
    | [] -> acc
    | (spec, token) :: xs ->
        begin match spec with
        | Inline -> begin
            if acc.inline then warn_duplicate spec token;
            helper xs { acc with inline = true }
          end
        | NoReturn -> begin
            if acc.no_return then warn_duplicate spec token;
            helper xs { acc with no_return = true }
          end
        end
  in
  helper specs Ast.function_specifiers_empty

(* -------------------- *)
(* --- Array Suffix --- *)
(* -------------------- *)

(* 
[ type-qualifier-list-opt assignment-expression-opt ]
[ static type-qualifier-list-opt assignment-expression ]
[ type-qualifier-list static assignment-expression ]
[ type-qualifier-list-opt * ] 
*)

and parse_declarator_array_suffix (parser : t) :
    Syntax.array_suffix parse_state_result =
  let emit_static_with_unspecified_length parser static_token =
    emit_error_token_loc parser static_token
      "'static' may not be used with an unspecified variable length"
  in

  let emit_static_with_no_size parser static_token =
    emit_error_token_loc parser static_token
      "'static' may not be used without an array size"
  in

  let emit_expected_expression parser token : unit parse_state_result =
    emit_error_token_loc parser token "expected expression";
    Error parser
  in

  let parse_size_with_static (parser : t) (static_token : Token.t) :
      Syntax.array_size parse_state_result =
    let next_token = peek parser in
    let next_parser = advance parser in

    match next_token.kind with
    (* TODO: change this to use assignment expression instead of IntLiteral *)
    | IntLiteral s -> Ok (next_parser, Size s)
    | Star ->
        emit_static_with_unspecified_length next_parser static_token;
        Ok (next_parser, NoSize)
    | RightBracket ->
        emit_static_with_no_size next_parser static_token;
        Ok (parser, NoSize)
    | _ ->
        let* _ = emit_expected_expression next_parser next_token in
        Ok (next_parser, Syntax.NoSize)
  in

  let parse_after_initial_static (parser : t) (static_token : Token.t) :
      Syntax.array_suffix parse_state_result =
    let parser, type_qualifiers = parse_type_qualifier_list parser in
    let* parser, size = parse_size_with_static parser static_token in
    let* parser, _ = expect parser RightBracket "expected ']'" in
    Ok
      ( parser,
        ({ size; type_qualifiers; is_static = true } : Syntax.array_suffix) )
  in

  let parse_after_no_initial_static (parser : t) :
      Syntax.array_suffix parse_state_result =
    let parser, type_qualifiers = parse_type_qualifier_list parser in

    let token = peek parser in
    let next_parser = advance parser in

    let* (parser, (type_qualifiers, size, is_static)) :
        t * (Syntax.type_qualifiers * Syntax.array_size * bool) =
      match token.kind with
      (* TODO: change this to use assignment expression instead of IntLiteral *)
      | IntLiteral s -> Ok (next_parser, (type_qualifiers, Syntax.Size s, false))
      | Static -> begin
          let* parser, size = parse_size_with_static next_parser token in
          Ok (parser, (type_qualifiers, size, true))
        end
      | Star -> Ok (next_parser, (type_qualifiers, Star, false))
      | _ -> Ok (parser, (type_qualifiers, NoSize, false))
    in

    let* parser, _ = expect parser RightBracket "expected ']'" in
    Ok (parser, ({ size; type_qualifiers; is_static } : Syntax.array_suffix))
  in

  let* parser, _ = expect parser LeftBracket "expected '['" in

  let initial_token = peek parser in
  if initial_token.kind = Static then
    parse_after_initial_static (advance parser) initial_token
  else parse_after_no_initial_static parser

(* ----------------------- *)
(* --- Function Suffix --- *)
(* ----------------------- *)

and parse_parameter_declaration (parser : t) :
    Syntax.function_param_declaration parse_state_result =
  let* parser, specs = parse_declaration_specifiers parser in
  match parse_declarator parser false with
  | Ok (parser, decl) -> Ok (parser, Syntax.Declaration (specs, decl))
  | Error (_, NoIdentifier) -> begin
      let next_token = peek parser in
      match next_token.kind with
      | Star | LeftParen | LeftBracket -> begin
          let* parser, decl = parse_abstract_declarator parser in
          Ok (parser, Syntax.AbstractDeclaration (specs, Some decl))
        end
      | _ -> Ok (parser, Syntax.AbstractDeclaration (specs, None))
    end
  | Error (parser, _) -> Error parser

and parse_parameter_type_list (parser : t) :
    Syntax.function_param_type_list parse_state_result =
  let rec parse_remaining_decls (parser : t)
      (acc : Syntax.function_param_declaration list) :
      (Syntax.function_param_declaration list * bool) parse_state_result =
    let next_parser, next_token = peek_and_advance parser in
    match next_token.kind with
    | Comma -> begin
        let parser_after_comma, token_after_comma =
          peek_and_advance next_parser
        in
        match token_after_comma.kind with
        | Ellipses -> Ok (parser_after_comma, (List.rev acc, true))
        | _ -> begin
            let* parser, decl = parse_parameter_declaration next_parser in
            parse_remaining_decls parser (decl :: acc)
          end
      end
    | _ -> Ok (parser, (List.rev acc, false))
  in

  let* parser, first_decl = parse_parameter_declaration parser in
  let* parser, (remaining_decls, has_ellipses) =
    parse_remaining_decls parser []
  in
  let parameter_type_list : Syntax.function_param_type_list =
    { declarators = first_decl :: remaining_decls; has_ellipses }
  in

  Ok (parser, parameter_type_list)

and parse_declarator_function_suffix (parser : t) :
    Syntax.function_suffix parse_state_result =
  let* parser, _ = expect parser LeftParen "expected '('" in
  let* parser, params = parse_parameter_type_list parser in
  let* parser, _ =
    expect parser RightParen "expected ')' after function parameters"
  in
  Ok (parser, Syntax.ParamList params)

(* ------------------------- *)
(* --- Declarator Suffix --- *)
(* ------------------------- *)

and parse_declarator_suffixes (parser : t) :
    Syntax.declarator_suffix list parse_state_result =
  let rec helper (parser : t) (acc : Syntax.declarator_suffix list) :
      Syntax.declarator_suffix list parse_state_result =
    match (peek parser).kind with
    | LeftBracket ->
        let* parser, suffix = parse_declarator_array_suffix parser in
        helper parser (ArraySuffix suffix :: acc)
    | LeftParen ->
        let* parser, suffix = parse_declarator_function_suffix parser in
        helper parser (FunctionSuffix suffix :: acc)
    | _ -> Ok (parser, acc)
  in
  helper parser []

(* ---------------------------------- *)
(* --- Abstract Declarator Suffix --- *)
(* ---------------------------------- *)

and parse_abstract_declarator_suffixes (parser : t) :
    Syntax.declarator_suffix list parse_state_result =
  let rec helper (parser : t) (acc : Syntax.declarator_suffix list) :
      Syntax.declarator_suffix list parse_state_result =
    match (peek parser).kind with
    | LeftBracket ->
        let* parser, suffix = parse_declarator_array_suffix parser in
        helper parser (ArraySuffix suffix :: acc)
    | LeftParen ->
        let* parser, suffix = parse_declarator_function_suffix parser in
        helper parser (FunctionSuffix suffix :: acc)
    | _ -> Ok (parser, acc)
  in
  helper parser []

(* -------------------*)
(* --- Type Names --- *)
(* -------------------*)

and parse_abstract_declarator (parser : t) :
    Syntax.abstract_declarator parse_state_result =
  let parse_suffixes (parser : t) (pointers : Syntax.pointers) :
      Syntax.abstract_declarator parse_state_result =
    let* parser, suffixes = parse_abstract_declarator_suffixes parser in
    let decl : Syntax.abstract_declarator =
      { pointers; decl_base = None; suffixes }
    in
    Ok (parser, decl)
  in

  let parse_left_paren (parser : t) (pointers : Syntax.pointers) :
      Syntax.abstract_declarator parse_state_result =
    let* parser, decl_base = parse_abstract_declarator parser in
    let* parser, _ = expect parser Token.RightParen "expected ')'" in
    let* parser, suffixes = parse_abstract_declarator_suffixes parser in
    let decl : Syntax.abstract_declarator =
      { pointers; decl_base = Some decl_base; suffixes }
    in
    Ok (parser, decl)
  in

  let* parser, pointers = parse_pointer_list parser in

  let next_parser, curr_token = peek_and_advance parser in
  match pointers with
  | [] ->
      begin match curr_token.kind with
      | LeftParen -> parse_left_paren next_parser pointers
      | LeftBracket -> parse_suffixes parser pointers
      | _ ->
          emit_error_token_loc next_parser curr_token
            "expected '*', '(', or '['";
          Error parser
      end
  | pointers ->
      begin match curr_token.kind with
      | LeftParen -> parse_left_paren next_parser pointers
      | LeftBracket -> parse_suffixes parser pointers
      | _ ->
          let* parser, suffixes = parse_abstract_declarator_suffixes parser in
          let decl : Syntax.abstract_declarator =
            { pointers; decl_base = None; suffixes }
          in
          Ok (parser, decl)
      end

and parse_type_name (parser : t) : Syntax.type_name parse_state_result =
  let* parser, specs = parse_specifier_qualifier_list parser in
  let curr_token = peek parser in

  let* (parser, decl) : t * Syntax.abstract_declarator option =
    match curr_token.kind with
    | Star | LeftParen | LeftBracket ->
        let* parser, decl = parse_abstract_declarator parser in
        Ok (parser, Some decl)
    | _ -> Ok (parser, None)
  in

  let type_name : Syntax.type_name =
    { specifier_qualifier_list = specs; decl }
  in
  Ok (parser, type_name)

(* ---------------------*)
(* --- Declarations --- *)
(* ---------------------*)

and parse_declarator (parser : t) (emit_errors : bool) :
    (t * Syntax.declarator, t * parse_declarator_error) result =
  let* parser, pointers =
    parse_pointer_list parser
    |> Result.map_error (fun parser -> (parser, NormalError))
  in

  let curr_token = peek parser in
  let next_parser = advance parser in

  match curr_token.kind with
  | Identifier name -> begin
      let decl_base : Syntax.declarator_base =
        Identifier { name; info = curr_token.info }
      in
      let* parser, suffixes =
        parse_declarator_suffixes next_parser
        |> Result.map_error (fun parser -> (parser, NormalError))
      in
      let decl : Syntax.declarator = { pointers; decl_base; suffixes } in
      Ok (parser, decl)
    end
  | LeftParen -> begin
      let* parser, decl_base = parse_declarator next_parser emit_errors in
      let decl_base : Syntax.declarator_base = Declarator decl_base in
      let* parser, _ =
        expect parser RightParen "expect ')'"
        |> Result.map_error (fun parser -> (parser, NormalError))
      in
      let* parser, suffixes =
        parse_declarator_suffixes parser
        |> Result.map_error (fun parser -> (parser, NormalError))
      in
      let decl : Syntax.declarator = { pointers; decl_base; suffixes } in
      Ok (parser, decl)
    end
  | _ ->
      if emit_errors then
        emit_error_token_span parser curr_token "expected identifier or '('";
      Error (parser, NoIdentifier)

let parse_declaration (parser : t) : Ast.declaration parse_state_result =
  let* parser, declaration_specifiers = parse_declaration_specifiers parser in
  let _ =
    analyze_function_storage_classes parser
      declaration_specifiers.storage_classes
  in
  let _ =
    analyze_type_qualifiers parser declaration_specifiers.type_qualifiers
  in
  let _ =
    analyze_function_specifiers parser declaration_specifiers.func_specifiers
  in

  let* parser, decl =
    parse_declarator parser true
    |> Result.map_error (fun decl_error ->
        match decl_error with parser, _ -> parser)
  in

  print_endline (Syntax.show_declaration_specifiers declaration_specifiers);
  print_endline (Syntax.show_declarator decl);
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
(* match parse_type_name parser with *)
(* | Ok _ -> () *)
(* | Error _ -> () *)
