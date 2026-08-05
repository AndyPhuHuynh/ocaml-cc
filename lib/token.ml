type header_type = Local | NonLocal
type header_name = { filepath : string; type_ : header_type }
type int_literal = { value : Z.t; suffix : string option }

type invalid =
  | UnterminatedCharLiteral
  | UnterminatedComment
  | UnterminatedHeaderName
  | UnterminatedStringLiteral
  | InvalidChar of char

type kind =
  (* Preprocessing *)
  | HeaderName of header_name
  | PPChar of Source.string_src
  | PPNumber of string
  | PPString of Source.string_src
  (* Keywords *)
  | Auto
  | Break
  | Case
  | Char
  | Const
  | Continue
  | Default
  | Do
  | Double
  | Else
  | Enum
  | Extern
  | Float
  | For
  | Goto
  | If
  | Inline
  | Int
  | Long
  | Register
  | Restrict
  | Return
  | Short
  | Signed
  | Sizeof
  | Static
  | Struct
  | Switch
  | Typedef
  | Union
  | Unsigned
  | Void
  | Volatile
  | While
  (* _Keywords *)
  | Bool
  | Complex
  | Imaginary
  (* Identifiers and literals *)
  | Identifier of string
  | CharLiteral of string
  | IntLiteral of int_literal
  | FloatLiteral of float
  | StringLiteral of string
  (* Operators *)
  | Plus
  | PlusEqual
  | PlusPlus
  | Minus
  | MinusEqual
  | MinusMinus
  | Arrow
  | Star
  | StarEqual
  | Slash
  | SlashEqual
  | Percent
  | PercentEqual
  | Equal
  | EqualEqual
  | Bang
  | BangEqual
  | Less
  | LessEqual
  | LessLess
  | LessLessEqual
  | Greater
  | GreaterEqual
  | GreaterGreater
  | GreaterGreaterEqual
  | And
  | AndEqual
  | AndAnd
  | Or
  | OrEqual
  | OrOr
  | Caret
  | CaretEqual
  | Tilde
  (* Punctuation *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Colon
  | Comma
  | Ellipses
  | Hash
  | HashHash
  | Semicolon
  | Period
  | Question
  (* Implementation *)
  | NewLine
  | Eof
  | Invalid of invalid

type t = {
  kind : kind;
  span : Source.span;
  line : int;
  col : int;
  is_at_line_start : bool;
}

let pp_header_type (fmt : Format.formatter) (type_ : header_type) =
  let str = match type_ with Local -> "Local" | NonLocal -> "NonLocal" in
  Format.fprintf fmt "%s" str

let display_source_positions (s : Source.string_pos list) : string =
  let rec helper (list : Source.string_pos list) (acc : string) : string =
    match list with
    | [] -> acc
    | { index; pos } :: xs -> begin
        let repr = Printf.sprintf "{ i: %d; pos: %d }" index pos in
        helper xs (acc ^ repr)
      end
  in

  helper s ""

let pp_kind_name (fmt : Format.formatter) (kind : kind) =
  match kind with
  (* Preprocessor *)
  | HeaderName { filepath; _ } -> Format.fprintf fmt "HeaderName(%S)" filepath
  | PPChar { string; _ } -> Format.fprintf fmt "PPChar(%S)" string
  | PPNumber value -> Format.fprintf fmt "PPNumber(%S)" value
  | PPString { string } -> Format.fprintf fmt "PPString(%S)" string
  (* Keywords *)
  | Auto -> Format.fprintf fmt "Auto"
  | Break -> Format.fprintf fmt "Break"
  | Case -> Format.fprintf fmt "Case"
  | Char -> Format.fprintf fmt "Char"
  | Const -> Format.fprintf fmt "Const"
  | Continue -> Format.fprintf fmt "Continue"
  | Default -> Format.fprintf fmt "Default"
  | Do -> Format.fprintf fmt "Do"
  | Double -> Format.fprintf fmt "Double"
  | Else -> Format.fprintf fmt "Else"
  | Enum -> Format.fprintf fmt "Enum"
  | Extern -> Format.fprintf fmt "Extern"
  | Float -> Format.fprintf fmt "Float"
  | For -> Format.fprintf fmt "For"
  | Goto -> Format.fprintf fmt "Goto"
  | If -> Format.fprintf fmt "If"
  | Inline -> Format.fprintf fmt "Inline"
  | Int -> Format.fprintf fmt "Int"
  | Long -> Format.fprintf fmt "Long"
  | Register -> Format.fprintf fmt "Register"
  | Restrict -> Format.fprintf fmt "Restrict"
  | Return -> Format.fprintf fmt "Return"
  | Short -> Format.fprintf fmt "Short"
  | Signed -> Format.fprintf fmt "Signed"
  | Sizeof -> Format.fprintf fmt "Sizeof"
  | Static -> Format.fprintf fmt "Static"
  | Struct -> Format.fprintf fmt "Struct"
  | Switch -> Format.fprintf fmt "Switch"
  | Typedef -> Format.fprintf fmt "Typedef"
  | Union -> Format.fprintf fmt "Union"
  | Unsigned -> Format.fprintf fmt "Unsigned"
  | Void -> Format.fprintf fmt "Void"
  | Volatile -> Format.fprintf fmt "Volatile"
  | While -> Format.fprintf fmt "While"
  (* _Keywords *)
  | Bool -> Format.fprintf fmt "_Bool"
  | Complex -> Format.fprintf fmt "_Complex"
  | Imaginary -> Format.fprintf fmt "_Imaginary"
  (* Identifiers and literals *)
  | Identifier str -> Format.fprintf fmt "Identifier(%S)" str
  | CharLiteral str -> Format.fprintf fmt "CharLiteral(%S)" str
  | IntLiteral i -> Format.fprintf fmt "IntLiteral(%a)" Z.pp_print i.value
  | FloatLiteral f -> Format.fprintf fmt "FloatLiteral(%f)" f
  | StringLiteral str -> Format.fprintf fmt "StringLiteral(%S)" str
  (* Operators *)
  | Plus -> Format.fprintf fmt "Plus"
  | PlusEqual -> Format.fprintf fmt "PlusEqual"
  | PlusPlus -> Format.fprintf fmt "PlusPlus"
  | Minus -> Format.fprintf fmt "Minus"
  | MinusEqual -> Format.fprintf fmt "MinusEqual"
  | MinusMinus -> Format.fprintf fmt "MinusMinus"
  | Arrow -> Format.fprintf fmt "Arrow"
  | Star -> Format.fprintf fmt "Star"
  | StarEqual -> Format.fprintf fmt "StarEqual"
  | Slash -> Format.fprintf fmt "Slash"
  | SlashEqual -> Format.fprintf fmt "SlashEqual"
  | Percent -> Format.fprintf fmt "Percent"
  | PercentEqual -> Format.fprintf fmt "PercentEqual"
  | Equal -> Format.fprintf fmt "Equal"
  | EqualEqual -> Format.fprintf fmt "EqualEqual"
  | Bang -> Format.fprintf fmt "Bang"
  | BangEqual -> Format.fprintf fmt "BangEqual"
  | Less -> Format.fprintf fmt "Less"
  | LessEqual -> Format.fprintf fmt "LessEqual"
  | LessLess -> Format.fprintf fmt "LessLess"
  | LessLessEqual -> Format.fprintf fmt "LessLessEqual"
  | Greater -> Format.fprintf fmt "Greater"
  | GreaterEqual -> Format.fprintf fmt "GreaterEqual"
  | GreaterGreater -> Format.fprintf fmt "GreaterGreater"
  | GreaterGreaterEqual -> Format.fprintf fmt "GreaterGreaterEqual"
  | And -> Format.fprintf fmt "And"
  | AndEqual -> Format.fprintf fmt "AndEqual"
  | AndAnd -> Format.fprintf fmt "AndAnd"
  | Or -> Format.fprintf fmt "Or"
  | OrEqual -> Format.fprintf fmt "OrEqual"
  | OrOr -> Format.fprintf fmt "OrOr"
  | Caret -> Format.fprintf fmt "Caret"
  | CaretEqual -> Format.fprintf fmt "CaretEqual"
  | Tilde -> Format.fprintf fmt "Tilde"
  (* Punctuation *)
  | LeftParen -> Format.fprintf fmt "LeftParen"
  | RightParen -> Format.fprintf fmt "RightParen"
  | LeftBrace -> Format.fprintf fmt "LeftBrace"
  | RightBrace -> Format.fprintf fmt "RightBrace"
  | LeftBracket -> Format.fprintf fmt "LeftBracket"
  | RightBracket -> Format.fprintf fmt "RightBracket"
  | Colon -> Format.fprintf fmt "Colon"
  | Comma -> Format.fprintf fmt "Comma"
  | Ellipses -> Format.fprintf fmt "Ellipses"
  | Hash -> Format.fprintf fmt "Hash"
  | HashHash -> Format.fprintf fmt "HashHash"
  | Semicolon -> Format.fprintf fmt "Semicolon"
  | Period -> Format.fprintf fmt "Period"
  | Question -> Format.fprintf fmt "Question"
  | NewLine -> Format.fprintf fmt "NewLine"
  | Eof -> Format.fprintf fmt "Eof"
  | Invalid invalid ->
      begin match invalid with
      | UnterminatedCharLiteral -> Format.fprintf fmt "UnterminatedCharLiteral"
      | UnterminatedComment -> Format.fprintf fmt "UnterminatedMultiLineComment"
      | UnterminatedHeaderName -> Format.fprintf fmt "UnterminatedHeaderName"
      | UnterminatedStringLiteral ->
          Format.fprintf fmt "UnterminatedStringLiteral"
      | InvalidChar c ->
          Format.fprintf fmt "InvalidCharacter(%S)" (String.make 1 c)
      end

let pp_kind_fields (fmt : Format.formatter) (kind : kind) =
  match kind with
  | HeaderName { filepath; type_ } ->
      Format.fprintf fmt "filepath: %S" filepath;
      Format.fprintf fmt "@,type: %a" pp_header_type type_
  | PPChar value ->
      Format.fprintf fmt "@[<v 2>";
      Format.fprintf fmt "splices:";
      Format.fprintf fmt "@,%a" Source.pp_string_pos_list value.positions;
      Format.fprintf fmt "@]"
  (* | PPNumber value -> Printf.sprintf "PPNumber {%s}" value *)
  | PPString value ->
      Format.fprintf fmt "@[<v 2>";
      Format.fprintf fmt "splices:";
      Format.fprintf fmt "@,%a" Source.pp_string_pos_list value.positions;
      Format.fprintf fmt "@]"
  | _ -> ()

let has_fields (kind : kind) : bool =
  match kind with
  | HeaderName _ | PPChar _ | PPNumber _ | PPString _ -> true
  | _ -> false

let should_print_lexeme (kind : kind) : bool =
  match kind with
  | Invalid UnterminatedCharLiteral
  | Invalid UnterminatedComment
  | Invalid UnterminatedStringLiteral ->
      false
  | _ -> true

let pp (manager : Source.manager) (fmt : Format.formatter) (token : t) : unit =
  Format.fprintf fmt "@[<v 2>";

  Format.fprintf fmt "%a" pp_kind_name token.kind;
  Format.fprintf fmt "@,loc: %d:%d" token.line token.col;

  if has_fields token.kind then begin
    Format.fprintf fmt "@,%a" pp_kind_fields token.kind
  end;

  if should_print_lexeme token.kind then begin
    Format.fprintf fmt "@,lexeme: %S" (Source.span_to_string token.span manager)
  end;

  Format.fprintf fmt "@]"
