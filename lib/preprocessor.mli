type t

val create : string -> (t, Source.load_error) result
val get_source_manager : t -> Source.manager
val next_token : t -> Token.t * t
