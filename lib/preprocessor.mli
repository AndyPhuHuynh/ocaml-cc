type t

val create : Source.load_type -> (t, Source.load_error) result

val create_with_manager :
  Source.load_type -> Source.manager -> (t, Source.load_error) result

val get_source_manager : t -> Source.manager
val next_token : t -> Token.t * t
