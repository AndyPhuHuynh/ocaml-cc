type string_error = InvalidEscape of { start : int; finish : int }
type convert_error = StringError of string_error list

let print_string_error (original_str : string) (err : string_error) : unit =
  match err with
  | InvalidEscape i ->
      Printf.printf "Invalid escape character at index %d-%d: %s\n" i.start
        i.finish original_str

let print_string_errors (original_str : string) (errors : string_error list) :
    unit =
  List.iter (fun e -> print_string_error original_str e) errors

let convert_string (s : string) : (string, string_error list) result =
  let len = String.length s in
  let buf = Buffer.create len in

  let rec helper (i : int) (errors : string_error list) :
      (string, string_error list) result =
    if i >= len then
      match errors with
      | [] -> Ok (Buffer.contents buf)
      | _ -> Error (List.rev errors)
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
          | _ ->
              Buffer.add_char buf c;
              helper (i + 2)
                (InvalidEscape { start = i; finish = i + 1 } :: errors)
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

let convert_token (token : Token.t) : (Token.t, convert_error) result =
  match token.kind with
  | Identifier name ->
      begin match keyword_of_string name with
      | Some kind -> Ok { token with kind }
      | None -> Ok token
      end
  | CharLiteral str ->
      begin match convert_string str with
      | Ok new_str -> Ok { token with kind = CharLiteral new_str }
      | Error errors ->
          print_string_errors str errors;
          Error (StringError errors)
      end
  | StringLiteral str -> begin
      begin match convert_string str with
      | Ok new_str -> Ok { token with kind = StringLiteral new_str }
      | Error errors ->
          print_string_errors str errors;
          Error (StringError errors)
      end
    end
  | _ -> Ok token
