type t

val init : string -> t
val next_token : t -> Token.t * t
val tokenize_all : string -> Token.t list * Source.manager
