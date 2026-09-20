type header_type = Local | NonLocal [@@deriving show]
type header_name = { filepath : string; type_ : header_type } [@@deriving show]

(**)
type preprocess_char_prefix = NoPrefix | Utf16 | Utf32 | WChar
[@@deriving show]

type preprocess_char = {
  prefix : preprocess_char_prefix;
  contents : Source.string_src;
}
[@@deriving show]

(**)
type preprocess_string_prefix = NoPrefix | Utf8 | Utf16 | Utf32 | WChar
[@@deriving show]

type preprocess_string = {
  prefix : preprocess_string_prefix;
  contents : Source.string_src;
}
[@@deriving show]

(**)
type int_suffix = U | L | UL | LL | ULL [@@deriving show]

type int_literal = { value : Bignum.Int.t; suffix : int_suffix option }
[@@deriving show]

(**)
type float_suffix = F | L [@@deriving show]

type float_literal = { value : Bignum.Float.t; suffix : float_suffix option }
[@@deriving show]

type invalid =
  | EmptyCharLiteral
  | UnterminatedCharLiteral
  | UnterminatedComment
  | UnterminatedHeaderName
  | UnterminatedStringLiteral
  | InvalidChar of char
[@@deriving show]

type kind_tag =
  (* Preprocessing *)
  | HeaderName
  | PPIdentifier
  | PPNumber
  | PPChar
  | PPString
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
  | Alignas
  | Alignof
  | Atomic
  | Bool
  | Complex
  | Generic
  | Imaginary
  | NoReturn
  | StaticAssert
  | ThreadLocal
  (* Identifiers and literals *)
  | Identifier
  | CharLiteral
  | IntLiteral
  | FloatLiteral
  | StringLiteral
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
  | Invalid
[@@deriving show]

type kind =
  (* Preprocessing *)
  | HeaderName of header_name
  | PPIdentifier of Source.string_src
  | PPNumber of Source.string_src
  | PPChar of preprocess_char
  | PPString of preprocess_string
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
  | Alignas
  | Alignof
  | Atomic
  | Bool
  | Complex
  | Generic
  | Imaginary
  | NoReturn
  | StaticAssert
  | ThreadLocal
  (* Identifiers and literals *)
  | Identifier of string
  | CharLiteral of string
  | IntLiteral of int_literal
  | FloatLiteral of float_literal
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
[@@deriving show]

type info = { span : Source.span; loc : Source.loc; is_at_line_start : bool }
[@@deriving show]

type t = { kind : kind; info : info } [@@deriving show]

let tag_of_kind (kind : kind) : kind_tag =
  match kind with
  (* Preprocessing *)
  | HeaderName _ -> HeaderName
  | PPIdentifier _ -> PPIdentifier
  | PPNumber _ -> PPNumber
  | PPChar _ -> PPChar
  | PPString _ -> PPString
  (* Keywords *)
  | Auto -> Auto
  | Break -> Break
  | Case -> Case
  | Char -> Char
  | Const -> Const
  | Continue -> Continue
  | Default -> Default
  | Do -> Do
  | Double -> Double
  | Else -> Else
  | Enum -> Enum
  | Extern -> Extern
  | Float -> Float
  | For -> For
  | Goto -> Goto
  | If -> If
  | Inline -> Inline
  | Int -> Int
  | Long -> Long
  | Register -> Register
  | Restrict -> Restrict
  | Return -> Return
  | Short -> Short
  | Signed -> Signed
  | Sizeof -> Sizeof
  | Static -> Static
  | Struct -> Struct
  | Switch -> Switch
  | Typedef -> Typedef
  | Union -> Union
  | Unsigned -> Unsigned
  | Void -> Void
  | Volatile -> Volatile
  | While -> While
  (* _Keywords *)
  | Alignas -> Alignas
  | Alignof -> Alignof
  | Atomic -> Atomic
  | Bool -> Bool
  | Complex -> Complex
  | Generic -> Generic
  | Imaginary -> Imaginary
  | NoReturn -> NoReturn
  | StaticAssert -> StaticAssert
  | ThreadLocal -> ThreadLocal
  (* Identifiers and literals *)
  | Identifier _ -> Identifier
  | CharLiteral _ -> CharLiteral
  | IntLiteral _ -> IntLiteral
  | FloatLiteral _ -> FloatLiteral
  | StringLiteral _ -> StringLiteral
  (* Operators *)
  | Plus -> Plus
  | PlusEqual -> PlusEqual
  | PlusPlus -> PlusPlus
  | Minus -> Minus
  | MinusEqual -> MinusEqual
  | MinusMinus -> MinusMinus
  | Arrow -> Arrow
  | Star -> Star
  | StarEqual -> StarEqual
  | Slash -> Slash
  | SlashEqual -> SlashEqual
  | Percent -> Percent
  | PercentEqual -> PercentEqual
  | Equal -> Equal
  | EqualEqual -> EqualEqual
  | Bang -> Bang
  | BangEqual -> BangEqual
  | Less -> Less
  | LessEqual -> LessEqual
  | LessLess -> LessLess
  | LessLessEqual -> LessLessEqual
  | Greater -> Greater
  | GreaterEqual -> GreaterEqual
  | GreaterGreater -> GreaterGreater
  | GreaterGreaterEqual -> GreaterGreaterEqual
  | And -> And
  | AndEqual -> AndEqual
  | AndAnd -> AndAnd
  | Or -> Or
  | OrEqual -> OrEqual
  | OrOr -> OrOr
  | Caret -> Caret
  | CaretEqual -> CaretEqual
  | Tilde -> Tilde
  (* Punctuation *)
  | LeftParen -> LeftParen
  | RightParen -> RightParen
  | LeftBrace -> LeftBrace
  | RightBrace -> RightBrace
  | LeftBracket -> LeftBracket
  | RightBracket -> RightBracket
  | Colon -> Colon
  | Comma -> Comma
  | Ellipses -> Ellipses
  | Hash -> Hash
  | HashHash -> HashHash
  | Semicolon -> Semicolon
  | Period -> Period
  | Question -> Question
  (* Implementation *)
  | NewLine -> NewLine
  | Eof -> Eof
  | Invalid _ -> Invalid

let pp_string ~(escaped : bool) (fmt : Format.formatter) (s : string) : unit =
  if escaped then Format.fprintf fmt "%S" s else Format.fprintf fmt "%s" s

let pp_string_len ~(escaped : bool) ~(padding : int) (fmt : Format.formatter)
    (s : string) : unit =
  if escaped then Format.fprintf fmt "%-*S" padding s
  else Format.fprintf fmt "%-*s" padding s

let pp_header_type (fmt : Format.formatter) (type_ : header_type) =
  let str = match type_ with Local -> "Local" | NonLocal -> "NonLocal" in
  Format.fprintf fmt "%s" str

let display_source_positions (s : Source.string_pos list) : string =
  let rec helper (list : Source.string_pos list) (acc : string) : string =
    match list with
    | [] -> acc
    | { index; loc } :: xs -> begin
        let repr =
          Printf.sprintf "{ i: %d; loc: %2d:%-3d }" index loc.line loc.col
        in
        helper xs (acc ^ repr)
      end
  in

  helper s ""

let pp_kind_name ?(escaped : bool = true) (fmt : Format.formatter) (kind : kind)
    =
  match kind with
  (* Preprocessor *)
  | HeaderName { filepath; _ } -> Format.fprintf fmt "HeaderName(%s)" filepath
  | PPIdentifier { string } -> Format.fprintf fmt "PPIdentifier(%s)" string
  | PPNumber { string; _ } -> Format.fprintf fmt "PPNumber(%s)" string
  | PPChar { contents; _ } ->
      Format.fprintf fmt "PPChar(%a)" (pp_string ~escaped) contents.string
  | PPString { contents; _ } ->
      Format.fprintf fmt "PPString(%a)" (pp_string ~escaped) contents.string
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
  | Alignas -> Format.fprintf fmt "Alignas"
  | Alignof -> Format.fprintf fmt "Alignof"
  | Atomic -> Format.fprintf fmt "Atomic"
  | Bool -> Format.fprintf fmt "Bool"
  | Complex -> Format.fprintf fmt "Complex"
  | Generic -> Format.fprintf fmt "Generic"
  | Imaginary -> Format.fprintf fmt "Imaginary"
  | NoReturn -> Format.fprintf fmt "NoReturn"
  | StaticAssert -> Format.fprintf fmt "StaticAssert"
  | ThreadLocal -> Format.fprintf fmt "ThreadLocal"
  (* Identifiers and literals *)
  | Identifier str -> Format.fprintf fmt "Identifier(%s)" str
  | IntLiteral i -> Format.fprintf fmt "IntLiteral(%a)" Z.pp_print i.value
  | FloatLiteral f -> Format.fprintf fmt "FloatLiteral(%a)" Q.pp_print f.value
  | CharLiteral str ->
      Format.fprintf fmt "CharLiteral(%a)" (pp_string ~escaped) str
  | StringLiteral str ->
      Format.fprintf fmt "StringLiteral(%a)" (pp_string ~escaped) str
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
      | EmptyCharLiteral -> Format.fprintf fmt "EmptyCharLiteral"
      | UnterminatedCharLiteral -> Format.fprintf fmt "UnterminatedCharLiteral"
      | UnterminatedComment -> Format.fprintf fmt "UnterminatedMultiLineComment"
      | UnterminatedHeaderName -> Format.fprintf fmt "UnterminatedHeaderName"
      | UnterminatedStringLiteral ->
          Format.fprintf fmt "UnterminatedStringLiteral"
      | InvalidChar c ->
          Format.fprintf fmt "InvalidCharacter(%S)" (String.make 1 c)
      end

let print_preprocess_char_prefix (fmt : Format.formatter)
    (prefix : preprocess_char_prefix) : unit =
  let prefix_str =
    match prefix with
    | NoPrefix -> "None"
    | Utf16 -> "Utf16"
    | Utf32 -> "Utf32"
    | WChar -> "Wchar"
  in
  Format.fprintf fmt "%-6s" prefix_str

let print_preprocessor_string_prefix (fmt : Format.formatter)
    (prefix : preprocess_string_prefix) : unit =
  let prefix_str =
    match prefix with
    | NoPrefix -> "None"
    | Utf8 -> "Utf8"
    | Utf16 -> "Utf16"
    | Utf32 -> "Utf32"
    | WChar -> "Wchar"
  in
  Format.fprintf fmt "%-6s" prefix_str

let print_int_suffix_opt (fmt : Format.formatter) (suffix : int_suffix option) :
    unit =
  let suffix_str =
    match suffix with
    | None -> "None"
    | Some U -> "U"
    | Some L -> "L"
    | Some UL -> "UL"
    | Some LL -> "LL"
    | Some ULL -> "ULL"
  in
  Format.fprintf fmt "%-5s" suffix_str

let pp_float_suffix_opt (fmt : Format.formatter) (suffix : float_suffix option)
    : unit =
  let suffix_str =
    match suffix with None -> "None" | Some F -> "F" | Some L -> "L"
  in
  Format.fprintf fmt "%-5s" suffix_str

let pp_kind_fields_compact (fmt : Format.formatter) (kind : kind) : unit =
  match kind with
  | PPChar { prefix } ->
      Format.fprintf fmt "prefix: %a" print_preprocess_char_prefix prefix
  | PPString { prefix } ->
      Format.fprintf fmt "prefix: %a" print_preprocessor_string_prefix prefix
  | IntLiteral { suffix; _ } ->
      Format.fprintf fmt "suffix: %a" print_int_suffix_opt suffix
  | FloatLiteral { suffix; _ } ->
      Format.fprintf fmt "suffix: %a" pp_float_suffix_opt suffix
  | _ -> ()

let pp_kind_fields_verbose (fmt : Format.formatter) (kind : kind) =
  let pp_splices_list (fmt : Format.formatter) (slices : Source.string_pos list)
      : unit =
    Format.fprintf fmt "@[<v 2>";
    Format.fprintf fmt "splices:";
    Format.fprintf fmt "@,%a" Source.pp_string_pos_list slices;
    Format.fprintf fmt "@]"
  in

  match kind with
  | HeaderName { filepath; type_ } ->
      Format.fprintf fmt "filepath: %s" filepath;
      Format.fprintf fmt "@,type: %a" pp_header_type type_
  | PPNumber value -> pp_splices_list fmt value.positions
  | PPChar value ->
      Format.fprintf fmt "prefix: %a" print_preprocess_char_prefix value.prefix;
      Format.fprintf fmt "@,%a" pp_splices_list value.contents.positions
  | PPString value ->
      Format.fprintf fmt "prefix: %a" print_preprocessor_string_prefix
        value.prefix;
      Format.fprintf fmt "@,%a" pp_splices_list value.contents.positions
  | IntLiteral { suffix; _ } ->
      Format.fprintf fmt "suffix: %a" print_int_suffix_opt suffix
  | FloatLiteral { suffix; _ } ->
      Format.fprintf fmt "suffix: %a" pp_float_suffix_opt suffix
  | _ -> ()

let has_fields (kind : kind) : bool =
  match kind with
  | HeaderName _ | PPChar _ | PPNumber _ | PPString _ | IntLiteral _
  | FloatLiteral _ ->
      true
  | _ -> false

let should_print_lexeme (kind : kind) : bool =
  match kind with
  | Invalid UnterminatedCharLiteral
  | Invalid UnterminatedComment
  | Invalid UnterminatedStringLiteral ->
      false
  | _ -> true

let pp_compact ?(escaped : bool = true) (manager : Source.manager)
    (fmt : Format.formatter) (token : t) : unit =
  let kind_str = Format.asprintf "%a" (pp_kind_name ~escaped) token.kind in
  Format.fprintf fmt "%2d:%-3d %-22s  " token.info.loc.line token.info.loc.col
    kind_str;

  if has_fields token.kind then begin
    Format.fprintf fmt "%a" pp_kind_fields_compact token.kind
  end;

  if should_print_lexeme token.kind then begin
    Format.fprintf fmt "lexeme=%a"
      (pp_string_len ~escaped ~padding:20)
      (Source.span_to_string token.info.span manager)
  end

let pp_verbose ?(escaped : bool = true) (manager : Source.manager)
    (fmt : Format.formatter) (token : t) : unit =
  Format.fprintf fmt "@[<v 2>";

  Format.fprintf fmt "%a" (pp_kind_name ~escaped) token.kind;
  Format.fprintf fmt "@,loc: %d:%d" token.info.loc.line token.info.loc.col;

  if has_fields token.kind then begin
    Format.fprintf fmt "@,%a" pp_kind_fields_verbose token.kind
  end;

  if should_print_lexeme token.kind then begin
    Format.fprintf fmt "@,lexeme: %a"
      (pp_string_len ~escaped ~padding:20)
      (Source.span_to_string token.info.span manager)
  end;

  Format.fprintf fmt "@]"

let pp_list_verbose ?(escaped : bool = true) (manager : Source.manager)
    (fmt : Format.formatter) (tokens : t list) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,@,")
    (pp_verbose ~escaped manager)
    fmt tokens

let pp_list_compact ?(escaped : bool = true) (manager : Source.manager)
    (fmt : Format.formatter) (tokens : t list) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
    (pp_compact ~escaped manager)
    fmt tokens
