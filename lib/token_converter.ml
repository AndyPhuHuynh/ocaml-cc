type invalid_escape_type = SeqNormal | HexNoDigits | HexTooLarge
type index_span = { start : int; finish : int }

type string_error_proto = {
  seq_type : invalid_escape_type;
  seq : string;
  indices : index_span;
}

type string_error = {
  seq_type : invalid_escape_type;
  seq : string;
  span : Source.span;
}

type pp_number_error_proto =
  | InvalidDigit of { digit : char; is_octal : bool; index : int }
  | InvalidSuffix of { suffix : string; indices : index_span }

type pp_number_error =
  | InvalidDigit of { digit : char; is_octal : bool; loc : Source.loc }
  | InvalidSuffix of { suffix : string; span : Source.span }

type conversion_error =
  | StringError of string_error list
  | PPNumberError of pp_number_error

type conversion_result =
  | Success of Token.t
  | Recovered of Token.t * conversion_error
  | Unrecoverable of conversion_error

let print_string_error (source : Source.t) (e : string_error) : unit =
  let emit_fn, msg =
    match e.seq_type with
    | SeqNormal ->
        ( Diagnostics.emit_warning,
          Printf.sprintf "unknown escape sequence '%s'" e.seq )
    | HexNoDigits ->
        (Diagnostics.emit_error, "\\x used with no following hex digits")
    | HexTooLarge -> (Diagnostics.emit_error, "hex escape sequence out of range")
  in
  emit_fn (Diagnostics.from_span source e.span msg)

let print_pp_number_error (source : Source.t) (e : pp_number_error) : unit =
  match e with
  | InvalidDigit { digit; is_octal; loc } ->
      let type_ = if is_octal then "octal" else "integer" in
      let msg =
        Printf.sprintf "invalid digit '%c' in %s constant" digit type_
      in
      Diagnostics.emit_error (Diagnostics.at source loc msg)
  | InvalidSuffix { suffix; span } ->
      let msg =
        Printf.sprintf "invalid suffix '%s' on integer constant" suffix
      in
      Diagnostics.emit_error (Diagnostics.from_span source span msg)

let print_conversion_error (source : Source.t) (e : conversion_error) : unit =
  match e with
  | StringError errors ->
      List.iter (fun e -> print_string_error source e) errors
  | PPNumberError err -> print_pp_number_error source err

let convert_string_error (e : string_error_proto) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : string_error =
  let start =
    Source.string_index_to_source_pos source e.indices.start positions
  in
  let finish =
    Source.string_index_to_source_pos source e.indices.finish positions
  in
  let length = finish - start + 1 in
  { seq_type = e.seq_type; seq = e.seq; span = { source_id; start; length } }

let convert_string_errors (es : string_error_proto list) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : string_error list
    =
  List.map (fun e -> convert_string_error e source_id source positions) es

let convert_pp_number_error (e : pp_number_error_proto) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : pp_number_error =
  match e with
  | InvalidDigit { digit; is_octal; index } ->
      let loc = Source.string_index_to_source_loc source index positions in
      InvalidDigit { digit; is_octal; loc }
  | InvalidSuffix { suffix; indices = { start; finish } } ->
      let start = Source.string_index_to_source_pos source start positions in
      let finish = Source.string_index_to_source_pos source finish positions in
      let length = finish - start + 1 in
      InvalidSuffix { suffix; span = { source_id; start; length } }

let convert_string (s : string) : string * string_error_proto list =
  let len = String.length s in
  let buf = Buffer.create len in

  let rec skip_octal_characters (i : int) (depth : int) =
    if depth >= 3 then i
    else
      match s.[i] with
      | '0' .. '7' -> skip_octal_characters (i + 1) (depth + 1)
      | _ -> i
  in

  let rec skip_hex_characters (i : int) : int =
    if i >= len then i
    else
      match s.[i] with
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> skip_hex_characters (i + 1)
      | _ -> i
  in

  let rec helper (i : int) (errors : string_error_proto list) :
      string * string_error_proto list =
    if i >= len then (Buffer.contents buf, List.rev errors)
    else
      match s.[i] with
      | '\\' when i + 1 < len -> begin
          let c = s.[i + 1] in
          match c with
          | '\'' | '"' | '?' | '\\' ->
              Buffer.add_char buf c;
              helper (i + 2) errors
          | 'a' ->
              Buffer.add_char buf '\x07';
              helper (i + 2) errors
          | 'b' ->
              Buffer.add_char buf '\x08';
              helper (i + 2) errors
          | 't' ->
              Buffer.add_char buf '\x09';
              helper (i + 2) errors
          | 'n' ->
              Buffer.add_char buf '\x0a';
              helper (i + 2) errors
          | 'v' ->
              Buffer.add_char buf '\x0b';
              helper (i + 2) errors
          | 'f' ->
              Buffer.add_char buf '\x0c';
              helper (i + 2) errors
          | 'r' ->
              Buffer.add_char buf '\x0d';
              helper (i + 2) errors
          | 'e' ->
              Buffer.add_char buf '\x1b';
              helper (i + 2) errors
          | '0' .. '7' ->
              let octal_start = i + 1 in
              let octal_end = skip_octal_characters octal_start 0 in
              let octal_len = octal_end - octal_start in
              let seq = String.sub s octal_start octal_len in
              let value = Scanf.sscanf seq "%o" Fun.id in
              Buffer.add_char buf (char_of_int value);
              helper octal_end errors
          | 'x' ->
              let hex_start = i + 2 in
              let hex_end = skip_hex_characters hex_start in
              let hex_len = hex_end - hex_start in
              if hex_len = 0 then begin
                Buffer.add_char buf 'x';
                helper hex_end
                  ({
                     seq_type = HexNoDigits;
                     seq = "\\x";
                     indices = { start = i; finish = i + 1 };
                   }
                  :: errors)
              end
              else if hex_len > 2 then begin
                Buffer.add_char buf 'x';
                helper hex_end
                  ({
                     seq_type = HexTooLarge;
                     seq = "\\x";
                     indices = { start = i; finish = hex_end - 1 };
                   }
                  :: errors)
              end
              else begin
                let seq = String.sub s hex_start hex_len in
                let value = Scanf.sscanf seq "%x" Fun.id in
                Buffer.add_char buf (char_of_int value);
                helper hex_end errors
              end
          | c ->
              Buffer.add_char buf c;
              helper (i + 2)
                ({
                   seq_type = SeqNormal;
                   seq = Printf.sprintf "\\%c" c;
                   indices = { start = i; finish = i + 1 };
                 }
                :: errors)
        end
      | c ->
          Buffer.add_char buf c;
          helper (i + 1) errors
  in
  helper 0 []

let is_digit_decimal (c : char) : bool =
  match c with '0' .. '9' -> true | _ -> false

let is_digit_octal (c : char) : bool =
  match c with '0' .. '7' -> true | _ -> false

let is_digit_hex (c : char) : bool =
  match c with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

type parse_int_error =
  | MaybeFloat
  | PPNumberErrorProto of pp_number_error_proto

let parse_int_helper ~(base : int) ~(is_digit : char -> bool) (s : string)
    (digits_index : int) : (Token.int_literal, parse_int_error) result =
  let len = String.length s in

  let rec skip_digits (i : int) : int =
    if i >= len then i else if is_digit s.[i] then skip_digits (i + 1) else i
  in

  let contains_float_chars (i : int) : bool =
    if i >= len then false
    else match s.[i] with '.' | 'e' | 'E' | 'p' | 'P' -> true | _ -> false
  in

  let make_suffix_str (i : int) : string option =
    if i >= len then None else Some (String.sub s i (len - i))
  in

  let parse_suffix (s : string option) :
      (Token.int_suffix option, string) result =
    match s with
    | None -> Ok None
    | Some s ->
        begin match s with
        | "u" | "U" -> Ok (Some Token.U)
        | "l" | "L" -> Ok (Some Token.L)
        | "ll" | "LL" -> Ok (Some Token.LL)
        | "lu" | "lU" | "Lu" | "LU" -> Ok (Some Token.UL)
        | "ul" | "uL" | "Ul" | "UL" -> Ok (Some Token.UL)
        | "ull" | "uLL" | "Ull" | "ULL" -> Ok (Some Token.ULL)
        | "llu" | "llU" | "LLu" | "LLU" -> Ok (Some Token.ULL)
        | _ -> Error s
        end
  in

  let suffix_index = skip_digits digits_index in
  if contains_float_chars suffix_index then Error MaybeFloat
  else if suffix_index < len && is_digit_hex s.[suffix_index] then
    Error
      (PPNumberErrorProto
         (InvalidDigit
            {
              digit = s.[suffix_index];
              is_octal = base = 8;
              index = suffix_index;
            }))
  else begin
    let digits_str = String.sub s digits_index (suffix_index - digits_index) in
    let value = Z.of_string_base base digits_str in

    let suffix_str = make_suffix_str suffix_index in
    match parse_suffix suffix_str with
    | Ok suffix -> Ok { value; suffix }
    | Error suffix ->
        Error
          (PPNumberErrorProto
             (InvalidSuffix
                { suffix; indices = { start = suffix_index; finish = len - 1 } }))
  end

let parse_int_decimal = parse_int_helper ~base:10 ~is_digit:is_digit_decimal
let parse_int_octal = parse_int_helper ~base:8 ~is_digit:is_digit_octal
let parse_int_hex = parse_int_helper ~base:16 ~is_digit:is_digit_hex

let parse_int_literal (s : string) : (Token.int_literal, parse_int_error) result
    =
  if String.length s >= 2 && s.[0] = '0' then
    begin match s.[1] with
    | 'x' | 'X' -> parse_int_hex s 2
    | _ -> parse_int_octal s 1
    end
  else parse_int_decimal s 0

let convert_pp_number (s : string) : (Token.kind, pp_number_error_proto) result
    =
  match parse_int_literal s with
  | Ok literal -> Ok (Token.IntLiteral literal)
  | Error (PPNumberErrorProto err) -> Error err
  | Error MaybeFloat -> begin
      prerr_endline "Implement floats";
      exit 1
    end

let keyword_of_string (s : string) : Token.kind option =
  match s with
  (* Keywords *)
  | "auto" -> Some Auto
  | "break" -> Some Break
  | "case" -> Some Case
  | "char" -> Some Char
  | "const" -> Some Const
  | "continue" -> Some Continue
  | "default" -> Some Default
  | "do" -> Some Do
  | "double" -> Some Double
  | "else" -> Some Else
  | "enum" -> Some Enum
  | "extern" -> Some Extern
  | "float" -> Some Float
  | "for" -> Some For
  | "goto" -> Some Goto
  | "if" -> Some If
  | "inline" -> Some Inline
  | "int" -> Some Int
  | "long" -> Some Long
  | "register" -> Some Register
  | "restrict" -> Some Restrict
  | "return" -> Some Return
  | "short" -> Some Short
  | "signed" -> Some Signed
  | "sizeof" -> Some Sizeof
  | "static" -> Some Static
  | "struct" -> Some Struct
  | "switch" -> Some Switch
  | "typedef" -> Some Typedef
  | "union" -> Some Union
  | "unsigned" -> Some Unsigned
  | "void" -> Some Void
  | "volatile" -> Some Volatile
  | "while" -> Some While
  (* _Keywords *)
  | "_Bool" -> Some Bool
  | "_Complex" -> Some Complex
  | "_Imaginary" -> Some Imaginary
  | _ -> None

let convert_token (token : Token.t) (manager : Source.manager) :
    conversion_result =
  let source_id = token.span.source_id in
  let source = Source.get_source manager source_id in

  match token.kind with
  | Identifier name ->
      begin match keyword_of_string name with
      | Some kind -> Success { token with kind }
      | None -> Success token
      end
  | PPChar value -> begin
      begin match convert_string value.string with
      | new_str, [] -> Success { token with kind = CharLiteral new_str }
      | new_str, errors ->
          Recovered
            ( { token with kind = CharLiteral new_str },
              StringError
                (convert_string_errors errors source_id source value.positions)
            )
      end
    end
  | PPNumber value ->
      begin match convert_pp_number value.string with
      | Ok kind -> Success { token with kind }
      | Error err ->
          Unrecoverable
            (PPNumberError
               (convert_pp_number_error err source_id source value.positions))
      end
  | PPString value -> begin
      begin match convert_string value.string with
      | new_str, [] -> Success { token with kind = StringLiteral new_str }
      | new_str, errors ->
          Recovered
            ( { token with kind = StringLiteral new_str },
              StringError
                (convert_string_errors errors source_id source value.positions)
            )
      end
    end
  | _ -> Success token
