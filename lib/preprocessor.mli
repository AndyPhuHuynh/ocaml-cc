type t

val create : string -> (t, Source.load_error) result
val next_token : t -> Token.t * t

val tokenize_all :
  string -> (Token.t list * Source.manager, Source.load_error) result
