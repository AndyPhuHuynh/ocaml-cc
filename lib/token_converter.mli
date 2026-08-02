type string_error = InvalidEscape of { seq : string; span : Source.span }
type conversion_error = StringError of string_error list

type conversion_result =
  | Success of Token.t
  | Recovered of Token.t * conversion_error
  | Unrecoverable of conversion_error

val convert_token : Token.t -> Source.manager -> conversion_result
