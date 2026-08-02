type string_error_proto =
  | P_InvalidEscape of { seq : string; start : int; finish : int }

type string_error = InvalidEscape of { seq : string; span : Source.span }
type conversion_error = StringError of string_error list

type conversion_result =
  | Success of Token.t
  | Recovered of Token.t * conversion_error
  | Unrecoverable of conversion_error

let convert_string_error (e : string_error_proto) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : string_error =
  match e with
  | P_InvalidEscape e ->
      let start = Source.string_index_to_source_pos e.start positions in
      let finish = Source.string_index_to_source_pos e.finish positions in
      let length = finish - start + 1 in
      InvalidEscape { seq = e.seq; span = { source_id; start; length } }

let convert_string_errors (es : string_error_proto list) (source_id : Source.id)
    (source : Source.t) (positions : Source.string_pos list) : string_error list
    =
  List.map (fun e -> convert_string_error e source_id source positions) es

let convert_string (s : string) : string * string_error_proto list =
  let len = String.length s in
  let buf = Buffer.create len in

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
          | c ->
              Buffer.add_char buf c;
              helper (i + 2)
                (P_InvalidEscape
                   { seq = Printf.sprintf "\\%c" c; start = i; finish = i + 1 }
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
