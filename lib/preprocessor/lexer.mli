type t

val init : string -> t
val advance_token : t -> Token.t * t
val tokenize_all : string -> Token.t list
