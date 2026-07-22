type t

val create : Source.id -> Source.t -> t
val lex_token : t -> Token.t * t
val lex_header_name : t -> Token.t * t
val tokenize_all : Source.id -> Source.t -> Token.t list
