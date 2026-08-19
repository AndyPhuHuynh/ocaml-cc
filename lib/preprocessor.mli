type t

val create :
  Source.load_file -> Diagnostics.engine -> (t, Source.load_error) result

val get_source_manager : t -> Source.manager
val next_token : t -> Token.t * t
