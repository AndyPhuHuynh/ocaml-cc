type t

val create : Source.load_type -> (t, Source.load_error) result
val get_source_manager : t -> Source.manager
val next_token : t -> Token.t * t
