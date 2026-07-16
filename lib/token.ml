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
  | IntLiteral of int
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

type t = { kind : kind; line : int; col : int }

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
  | Identifier str -> str
  | CharLiteral c -> String.make 1 c
  | IntLiteral i -> string_of_int i
  | FloatLiteral f -> string_of_float f
  | StringLiteral str -> str
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

let to_string token =
  Printf.sprintf "(%s, %d:%d)" (kind_to_string token.kind) token.line token.col
