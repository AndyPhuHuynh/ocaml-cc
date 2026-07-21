type t

val init : string -> string -> t
val lex_token : t -> Token.t * t
val lex_header_name : t -> Token.t * t
val tokenize_all : string -> string -> Token.t list
