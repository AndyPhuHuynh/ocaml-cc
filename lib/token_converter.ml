type invalid_escape_type = SeqNormal | HexNoDigits | HexTooLarge

type string_error_proto = {
  seq_type : invalid_escape_type;
  seq : string;
  start : int;
  finish : int;
}

type string_error = {
  seq_type : invalid_escape_type;
  seq : string;
  span : Source.span;
}

type conversion_error = StringError of string_error list

type conversion_result =
  | Success of Token.t
  | Recovered of Token.t * conversion_error
  | Unrecoverable of conversion_error

let convert_string_error (e : string_error_proto) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : string_error =
  let start = Source.string_index_to_source_pos e.start positions in
  let finish = Source.string_index_to_source_pos e.finish positions in
  let length = finish - start + 1 in
  { seq_type = e.seq_type; seq = e.seq; span = { source_id; start; length } }

let convert_string_errors (es : string_error_proto list) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : string_error list
    =
  List.map (fun e -> convert_string_error e source_id source positions) es

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
                     start = i;
                     finish = i + 1;
                   }
                  :: errors)
              end
              else if hex_len > 2 then begin
                Buffer.add_char buf 'x';
                helper hex_end
                  ({
                     seq_type = HexTooLarge;
                     seq = "\\x";
                     start = i;
                     finish = hex_end - 1;
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
                   start = i;
                   finish = i + 1;
                 }
                :: errors)
        end
      | c ->
          Buffer.add_char buf c;
          helper (i + 1) errors
  in
  helper 0 []

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
