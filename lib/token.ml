type int_literal = { value : Z.t; suffix : string option }
type invalid = UnterminatedComment | UnterminatedString | InvalidChar of char

type kind =
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
  | CharLiteral of char
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
  | Semicolon
  | Period
  | Question
  | Eof
  | Invalid of invalid

type span = { start : int; finish : int }
type t = { kind : kind; span : span; line : int; col : int }

let kind_to_string = function
  (* Keywords *)
  | Auto -> "auto"
  | Break -> "break"
  | Case -> "case"
  | Char -> "char"
  | Const -> "const"
  | Continue -> "continue"
  | Default -> "default"
  | Do -> "do"
  | Double -> "double"
  | Else -> "else"
  | Enum -> "enum"
  | Extern -> "extern"
  | Float -> "float"
  | For -> "for"
  | Goto -> "goto"
  | If -> "if"
  | Inline -> "inline"
  | Int -> "int"
  | Long -> "long"
  | Register -> "register"
  | Restrict -> "restrict"
  | Return -> "return"
  | Short -> "short"
  | Signed -> "signed"
  | Sizeof -> "sizeof"
  | Static -> "static"
  | Struct -> "struct"
  | Switch -> "switch"
  | Typedef -> "typedef"
  | Union -> "union"
  | Unsigned -> "unsigned"
  | Void -> "void"
  | Volatile -> "volatile"
  | While -> "while"
  (* _Keywords *)
  | Bool -> "_Bool"
  | Complex -> "_Complex"
  | Imaginary -> "_Imaginary"
  (* Identifiers and literals *)
  | Identifier str -> "Identifier"
  | CharLiteral c -> "Char Literal"
  | IntLiteral i -> "Int Literal"
  | FloatLiteral f -> "Float Literal"
  | StringLiteral str -> "String Literal"
  (* Operators *)
  | Plus -> "Plus"
  | PlusEqual -> "PlusEqual"
  | PlusPlus -> "PlusPlus"
  | Minus -> "Minus"
  | MinusEqual -> "MinusEqual"
  | MinusMinus -> "MinusMinus"
  | Arrow -> "Arrow"
  | Star -> "Star"
  | StarEqual -> "StarEqual"
  | Slash -> "Slash"
  | SlashEqual -> "SlashEqual"
  | Percent -> "Percent"
  | PercentEqual -> "PercentEqual"
  | Equal -> "Equal"
  | EqualEqual -> "EqualEqual"
  | Bang -> "Bang"
  | BangEqual -> "BangEqual"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | LessLess -> "LessLess"
  | LessLessEqual -> "LessLessEqual"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | GreaterGreater -> "GreaterGreater"
  | GreaterGreaterEqual -> "GreaterGreaterEqual"
  | And -> "And"
  | AndEqual -> "AndEqual"
  | AndAnd -> "AndAnd"
  | Or -> "Or"
  | OrEqual -> "OrEqual"
  | OrOr -> "OrOr"
  | Caret -> "Caret"
  | CaretEqual -> "CaretEqual"
  | Tilde -> "Tilde"
  (* Punctuation *)
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | LeftBracket -> "LeftBracket"
  | RightBracket -> "RightBracket"
  | Colon -> "Colon"
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | Period -> "Period"
  | Question -> "Question"
  | Eof -> "EOF"
  | Invalid invalid -> (
      match invalid with
      | UnterminatedComment -> "Unterminated multi-line comment"
      | UnterminatedString -> "Unterminated string"
      | InvalidChar c -> Printf.sprintf "Invalid character '%c'" c)

let span_to_string span source =
  String.sub source span.start (span.finish - span.start)

let to_string token source =
  match token.kind with
  | Invalid UnterminatedComment | Invalid UnterminatedString | Eof ->
      Printf.sprintf "([%s], %d:%d)"
        (kind_to_string token.kind)
        token.line token.col
  | _ ->
      Printf.sprintf "([%s]: [%s], %d:%d)"
        (kind_to_string token.kind)
        (span_to_string token.span source)
        token.line token.col
