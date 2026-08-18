type header_type = Local | NonLocal
type header_name = { filepath : string; type_ : header_type }

(**)
type pp_string_prefix = None | Utf8 | Utf16 | Utf32 | WChar
type pp_string = { prefix : pp_string_prefix; contents : Source.string_src }

(**)
type int_suffix = U | L | UL | LL | ULL
type int_literal = { value : Z.t; suffix : int_suffix option }

(**)
type float_suffix = F | L
type float_literal = { value : Q.t; suffix : float_suffix option }

type invalid =
  | EmptyCharLiteral
  | UnterminatedCharLiteral
  | UnterminatedComment
  | UnterminatedHeaderName
  | UnterminatedStringLiteral
  | InvalidChar of char

type kind =
  (* Preprocessing *)
  | HeaderName of header_name
  | PPIdentifier of Source.string_src
  | PPNumber of Source.string_src
  | PPChar of Source.string_src
  | PPString of pp_string
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

type t = {
  kind : kind;
  span : Source.span;
  loc : Source.loc;
  is_at_line_start : bool;
}

val pp_header_type : Format.formatter -> header_type -> unit
val pp_kind_name : ?escaped:bool -> Format.formatter -> kind -> unit

val pp_compact :
  ?escaped:bool -> Source.manager -> Format.formatter -> t -> unit

val pp_verbose :
  ?escaped:bool -> Source.manager -> Format.formatter -> t -> unit

val pp_list_compact :
  ?escaped:bool -> Source.manager -> Format.formatter -> t list -> unit

val pp_list_verbose :
  ?escaped:bool -> Source.manager -> Format.formatter -> t list -> unit
