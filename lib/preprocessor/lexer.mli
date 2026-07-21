type t

val init : string -> t
val lex_token : t -> Token.t * t
val lex_header_name : t -> Token.t * t
val tokenize_all : string -> Token.t list
