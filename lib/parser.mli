type t

val create :
  Source.load_file -> Diagnostics.engine -> (t, Source.load_error) result

val parse_translation_unit : t -> unit
