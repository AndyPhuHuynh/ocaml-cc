type invalid_ucn = NoDigits | Incomplete | InvalidCodePoint of string

type invalid_escape_type =
  | SeqNormal
  | OctalTooLarge
  | HexNoDigits
  | HexTooLarge
  | Ucn of invalid_ucn

type index_span = { start : int; finish : int }
type identifier_error_proto = { type_ : invalid_ucn; indices : index_span }
type identifier_error = { type_ : invalid_ucn; span : Source.span }

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
  | InvalidSuffix of { suffix : string; is_float : bool; indices : index_span }
  | ExponentNoDigits of { index : int }
  | HexFloatNoExponent of { end_index : int }
  | HexFloatNoSignificand of { dot_index : int }

type pp_number_error =
  | InvalidDigit of { digit : char; is_octal : bool; loc : Source.loc }
  | InvalidSuffix of { suffix : string; is_float : bool; span : Source.span }
  | ExponentNoDigits of { loc : Source.loc }
  | HexFloatNoExponent of { loc : Source.loc }
  | HexFloatNoSignificand of { loc : Source.loc }

type conversion_error =
  | IdentifierError of identifier_error list
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
    | OctalTooLarge ->
        (Diagnostics.emit_error, "octal escape sequence out of range")
    | HexNoDigits ->
        (Diagnostics.emit_error, "\\x used with no following hex digits")
    | HexTooLarge -> (Diagnostics.emit_error, "hex escape sequence out of range")
    | Ucn err ->
        begin match err with
        | NoDigits ->
            (Diagnostics.emit_error, "\\u used with no following hex digits")
        | Incomplete ->
            (Diagnostics.emit_error, "incomplete universal character name")
        | InvalidCodePoint str ->
            ( Diagnostics.emit_error,
              Printf.sprintf
                "character <%s> cannot be specified by a universal character \
                 name"
                str )
        end
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
  | InvalidSuffix { suffix; is_float; span } ->
      let type_ = if is_float then "floating" else "integer" in
      let msg =
        Printf.sprintf "invalid suffix '%s' on %s constant" suffix type_
      in
      Diagnostics.emit_error (Diagnostics.from_span source span msg)
  | ExponentNoDigits { loc } ->
      let msg = "exponent has no digits" in
      Diagnostics.emit_error (Diagnostics.at source loc msg)
  | HexFloatNoExponent { loc } ->
      let msg = "hexadecimal floating constant requires an exponent" in
      Diagnostics.emit_error (Diagnostics.at source loc msg)
  | HexFloatNoSignificand { loc } ->
      let msg = "hexadecimal floating constant requires a significand" in
      Diagnostics.emit_error (Diagnostics.at source loc msg)

let print_conversion_error (source : Source.t) (e : conversion_error) : unit =
  match e with
  | IdentifierError errors ->
      List.iter
        (fun e ->
          let emit_fn, msg =
            match e.type_ with
            | NoDigits ->
                (Diagnostics.emit_error, "\\u used with no following hex digits")
            | Incomplete ->
                (Diagnostics.emit_error, "incomplete universal character name")
            | InvalidCodePoint str ->
                ( Diagnostics.emit_error,
                  Printf.sprintf
                    "character <%s> cannot be specified by a universal \
                     character name"
                    str )
          in
          emit_fn (Diagnostics.from_span source e.span msg))
        errors
  | StringError errors ->
      List.iter (fun e -> print_string_error source e) errors
  | PPNumberError err -> print_pp_number_error source err

let convert_indices_to_span (indices : index_span) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : Source.span =
  let start =
    Source.string_index_to_source_pos source indices.start positions
  in
  let finish =
    Source.string_index_to_source_pos source indices.finish positions
  in
  let length = finish - start + 1 in
  { source_id; start; length }

let convert_identifier_error (e : identifier_error_proto)
    (source_id : Source.id) (source : Source.t)
    (positions : Source.string_pos list) : identifier_error =
  let span = convert_indices_to_span e.indices source_id source positions in
  { type_ = e.type_; span }

let convert_identifier_errors (es : identifier_error_proto list)
    (source_id : Source.id) (source : Source.t)
    (positions : Source.string_pos list) : identifier_error list =
  List.map (fun e -> convert_identifier_error e source_id source positions) es

let convert_string_error (e : string_error_proto) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : string_error =
  let span = convert_indices_to_span e.indices source_id source positions in
  { seq_type = e.seq_type; seq = e.seq; span }

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
  | InvalidSuffix { suffix; is_float; indices } ->
      let span = convert_indices_to_span indices source_id source positions in
      InvalidSuffix { suffix; is_float; span }
  | ExponentNoDigits { index } ->
      let loc = Source.string_index_to_source_loc source index positions in
      ExponentNoDigits { loc }
  | HexFloatNoExponent { end_index } ->
      let loc = Source.string_index_to_source_loc source end_index positions in
      let loc = { loc with col = loc.col + 1 } in
      HexFloatNoExponent { loc }
  | HexFloatNoSignificand { dot_index } ->
      let loc = Source.string_index_to_source_loc source dot_index positions in
      let loc = { loc with col = loc.col + 1 } in
      HexFloatNoSignificand { loc }

let parse_ucn (s : string) (i : int) (is_eight_digits : bool) :
    (Uchar.t, invalid_ucn) result * int =
  let len = String.length s in

  let check_hex (i : int) : bool * int =
    let rec helper (max_depth : int) (i : int) (depth : int) : bool * int =
      if depth >= max_depth then (true, i)
      else if i >= len then (false, i)
      else
        begin match s.[i] with
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ->
            helper max_depth (i + 1) (depth + 1)
        | _ -> (false, i)
        end
    in

    let max_depth = if is_eight_digits then 8 else 4 in
    helper max_depth i 0
  in

  let parse_cp (start : int) (finish : int) : (Uchar.t, invalid_ucn) result =
    let cp_str = String.sub s start (finish - start) in
    let cp_num = Scanf.sscanf cp_str "%x" Fun.id in
    if not (Uchar.is_valid cp_num) then
      Error (InvalidCodePoint (Printf.sprintf "U+%s" cp_str))
    else begin
      let uchar = Uchar.of_int cp_num in
      if Unicode.ucn_continue_allowed uchar then Ok uchar
      else Error (InvalidCodePoint (Printf.sprintf "U+%s" cp_str))
    end
  in

  match check_hex i with
  | false, new_index ->
      if new_index = i then (Error NoDigits, new_index)
      else (Error Incomplete, new_index)
  | true, new_index -> (parse_cp i new_index, new_index)

let keyword_of_string (s : string) : Token.kind =
  match s with
  (* Keywords *)
  | "auto" -> Auto
  | "break" -> Break
  | "case" -> Case
  | "char" -> Char
  | "const" -> Const
  | "continue" -> Continue
  | "default" -> Default
  | "do" -> Do
  | "double" -> Double
  | "else" -> Else
  | "enum" -> Enum
  | "extern" -> Extern
  | "float" -> Float
  | "for" -> For
  | "goto" -> Goto
  | "if" -> If
  | "inline" -> Inline
  | "int" -> Int
  | "long" -> Long
  | "register" -> Register
  | "restrict" -> Restrict
  | "return" -> Return
  | "short" -> Short
  | "signed" -> Signed
  | "sizeof" -> Sizeof
  | "static" -> Static
  | "struct" -> Struct
  | "switch" -> Switch
  | "typedef" -> Typedef
  | "union" -> Union
  | "unsigned" -> Unsigned
  | "void" -> Void
  | "volatile" -> Volatile
  | "while" -> While
  (* _Keywords *)
  | "_Alignas" -> Alignas
  | "_Alignof" -> Alignof
  | "_Atomic" -> Atomic
  | "_Bool" -> Bool
  | "_Complex" -> Complex
  | "_Generic" -> Generic
  | "_Imaginary" -> Imaginary
  | "_Noreturn" -> NoReturn
  | "_Static_assert" -> StaticAssert
  | "_Thread_local" -> ThreadLocal
  | s -> Identifier s

let convert_identifier (s : string) :
    (Token.kind, identifier_error_proto list) result =
  let len = String.length s in

  let rec transform_string (i : int) (buf : Buffer.t)
      (errors : identifier_error_proto list) :
      (string, identifier_error_proto list) result =
    if i >= len then
      match errors with
      | [] -> Ok (Buffer.contents buf)
      | _ -> Error (List.rev errors)
    else
      match s.[i] with
      | '\\' ->
          begin match s.[i + 1] with
          | ('u' | 'U') as c -> begin
              let is_eight = c = 'U' in
              match parse_ucn s (i + 2) is_eight with
              | Ok uchar, new_index ->
                  Buffer.add_utf_8_uchar buf uchar;
                  transform_string new_index buf errors
              | Error err, new_index -> begin
                  transform_string new_index buf
                    ({
                       type_ = err;
                       indices = { start = i; finish = new_index - 1 };
                     }
                    :: errors)
                end
            end
          | _ -> failwith "invalid backslash in identifier"
          end
      | c ->
          Buffer.add_char buf c;
          transform_string (i + 1) buf errors
  in

  let ( let* ) = Result.bind in
  let* str = transform_string 0 (Buffer.create 16) [] in
  Ok (keyword_of_string str)

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
      | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> skip_hex_characters (i + 1)
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

              if value > 255 then begin
                let seq_str = String.sub s octal_start octal_len in
                Buffer.add_string buf seq_str;
                helper octal_end
                  ({
                     seq_type = OctalTooLarge;
                     seq = seq_str;
                     indices = { start = octal_start; finish = octal_end };
                   }
                  :: errors)
              end
              else begin
                Buffer.add_char buf (char_of_int value);
                helper octal_end errors
              end
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
          | 'u' | 'U' ->
              let is_eight_digits = c = 'U' in
              begin match parse_ucn s (i + 2) is_eight_digits with
              | Error err, new_index ->
                  helper new_index
                    ({
                       seq_type = Ucn err;
                       seq = "\\u";
                       indices = { start = i; finish = new_index - 1 };
                     }
                    :: errors)
              | Ok uchar, new_index ->
                  Buffer.add_utf_8_uchar buf uchar;
                  helper new_index errors
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
                {
                  suffix;
                  is_float = false;
                  indices = { start = suffix_index; finish = len - 1 };
                }))
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

let parse_decimal_float_literal (s : string) :
    (Token.float_literal, pp_number_error_proto) result =
  let len = String.length s in
  let rec skip_number (i : int) (dot_encountered : bool) : int =
    if i >= len then i
    else
      match s.[i] with
      | '.' when not dot_encountered -> skip_number (i + 1) true
      | '0' .. '9' -> skip_number (i + 1) dot_encountered
      | _ -> i
  in

  let rec skip_digits (i : int) =
    if i >= len then i
    else match s.[i] with '0' .. '9' -> skip_digits (i + 1) | _ -> i
  in

  let skip_exponent (i : int) =
    if i >= len then i
    else
      match s.[i] with
      | 'e' | 'E' ->
          if i + 1 >= len then i
          else
            begin match s.[i + 1] with
            | '+' | '-' -> if i + 2 >= len then i else skip_digits i
            | _ -> skip_digits (i + 1)
            end
      | _ -> i
  in

  let parse_suffix (i : int) :
      (Token.float_suffix option, pp_number_error_proto) result =
    if i >= len then Ok None
    else begin
      let suffix_str = String.sub s i (len - i) in
      match suffix_str with
      | _ when suffix_str.[0] = 'e' || suffix_str.[0] = 'E' ->
          Error (ExponentNoDigits { index = i })
      | "f" | "F" -> Ok (Some Token.F)
      | "l" | "L" -> Ok (Some Token.L)
      | _ ->
          Error
            (InvalidSuffix
               {
                 suffix = suffix_str;
                 is_float = true;
                 indices = { start = i; finish = len - i };
               })
    end
  in

  let suffix_index = skip_number 0 false in
  let suffix_index = skip_exponent suffix_index in
  match parse_suffix suffix_index with
  | Ok suffix ->
      let num_str = String.sub s 0 suffix_index in
      let value = Q.of_string num_str in
      Ok { value; suffix }
  | Error err -> Error err

let parse_hex_float_literal (s : string) :
    (Token.float_literal, pp_number_error_proto) result =
  let len = String.length s in

  let get_num_str () : (string * int * int, pp_number_error_proto) result =
    let rec acc_str (i : int) (num_digits_after_dot : int) (buf : Buffer.t)
        dot_encountered : string * int * int =
      if i >= len then (Buffer.contents buf, num_digits_after_dot, i)
      else
        match s.[i] with
        | '.' when not dot_encountered ->
            acc_str (i + 1) num_digits_after_dot buf true
        | c when is_digit_hex c ->
            Buffer.add_char buf c;
            let num_digits_after_dot =
              if dot_encountered then num_digits_after_dot + 1
              else num_digits_after_dot
            in
            acc_str (i + 1) num_digits_after_dot buf dot_encountered
        | _ -> (Buffer.contents buf, num_digits_after_dot, i)
    in
    let num_str, num_digits_after_dot, suffix_index =
      acc_str 2 0 (Buffer.create 8) false
    in
    if num_str = "" then
      Error (HexFloatNoSignificand { dot_index = suffix_index - 1 })
    else Ok (num_str, num_digits_after_dot, suffix_index)
  in

  let get_exp_str (i : int) :
      (string * bool * int, pp_number_error_proto) result =
    let rec acc_digits_str (i : int) (buf : Buffer.t) : string * int =
      if i >= len then (Buffer.contents buf, i)
      else
        match s.[i] with
        | c when is_digit_hex c ->
            Buffer.add_char buf c;
            acc_digits_str (i + 1) buf
        | _ -> (Buffer.contents buf, i)
    in

    if i >= len then Error (HexFloatNoExponent { end_index = i - 1 })
    else
      match s.[i] with
      | 'p' | 'P' ->
          if i + 1 >= len then Error (HexFloatNoExponent { end_index = i - 1 })
          else
            begin match s.[i + 1] with
            | ('+' | '-') as c ->
                if i + 2 >= len then
                  Error (HexFloatNoExponent { end_index = i - 1 })
                else begin
                  let str, suffix_index =
                    acc_digits_str (i + 2) (Buffer.create 4)
                  in
                  let is_neg = c = '-' in
                  Ok (str, is_neg, suffix_index)
                end
            | _ ->
                let str, suffix_index =
                  acc_digits_str (i + 1) (Buffer.create 4)
                in
                Ok (str, false, suffix_index)
            end
      | _ -> Error (HexFloatNoExponent { end_index = i - 1 })
  in

  let parse_suffix (i : int) :
      (Token.float_suffix option, pp_number_error_proto) result =
    if i >= len then Ok None
    else begin
      let suffix_str = String.sub s i (len - i) in
      match suffix_str with
      | _ when suffix_str.[0] = 'p' || suffix_str.[0] = 'P' ->
          Error (ExponentNoDigits { index = i })
      | "f" | "F" -> Ok (Some Token.F)
      | "l" | "L" -> Ok (Some Token.L)
      | _ ->
          Error
            (InvalidSuffix
               {
                 suffix = suffix_str;
                 is_float = true;
                 indices = { start = i; finish = len - i };
               })
    end
  in

  let make_num (num_str : string) (num_digits_after_dot : int)
      ((exp_str, is_negative) : string * bool) : Q.t =
    let two_exp : int =
      let exp_num = int_of_string exp_str in
      let exp_num = if is_negative then -exp_num else exp_num in
      (4 * -num_digits_after_dot) + exp_num
    in

    let num = Z.of_string_base 16 num_str in
    let num : Q.t = Q.of_bigint num in

    let res =
      if two_exp > 0 then Q.mul_2exp num two_exp else Q.div_2exp num (-two_exp)
    in
    res
  in

  let ( let* ) = Result.bind in
  let* num_str, num_digits_after_dot, suffix_index = get_num_str () in
  let* exp_str, is_negative, suffix_index = get_exp_str suffix_index in
  let* suffix = parse_suffix suffix_index in
  let value : Q.t =
    make_num num_str num_digits_after_dot (exp_str, is_negative)
  in
  let res : Token.float_literal = { value; suffix } in
  Ok res

let parse_float_literal (s : string) :
    (Token.float_literal, pp_number_error_proto) result =
  if String.length s >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then
    parse_hex_float_literal s
  else parse_decimal_float_literal s

let convert_pp_number (s : string) : (Token.kind, pp_number_error_proto) result
    =
  match parse_int_literal s with
  | Ok literal -> Ok (Token.IntLiteral literal)
  | Error (PPNumberErrorProto err) -> Error err
  | Error MaybeFloat ->
      begin match parse_float_literal s with
      | Ok literal -> Ok (Token.FloatLiteral literal)
      | Error err -> Error err
      end

let convert_token (token : Token.t) (manager : Source.manager) :
    conversion_result =
  let source_id = token.span.source_id in
  let source = Source.get_source manager source_id in

  match token.kind with
  | PPIdentifier value ->
      begin match convert_identifier value.string with
      | Ok kind -> Success { token with kind }
      | Error errors ->
          Unrecoverable
            (IdentifierError
               (convert_identifier_errors errors source_id source
                  value.positions))
      end
  | PPNumber value ->
      begin match convert_pp_number value.string with
      | Ok kind -> Success { token with kind }
      | Error err ->
          Unrecoverable
            (PPNumberError
               (convert_pp_number_error err source_id source value.positions))
      end
  | PPChar { prefix; contents = { string; positions } } -> begin
      begin match convert_string string with
      | new_str, [] -> Success { token with kind = CharLiteral new_str }
      | new_str, errors ->
          Recovered
            ( { token with kind = CharLiteral new_str },
              StringError
                (convert_string_errors errors source_id source positions) )
      end
    end
  | PPString { prefix; contents = { string; positions } } -> begin
      begin match convert_string string with
      | new_str, [] -> Success { token with kind = StringLiteral new_str }
      | new_str, errors ->
          Recovered
            ( { token with kind = StringLiteral new_str },
              StringError
                (convert_string_errors errors source_id source positions) )
      end
    end
  | _ -> Success token
