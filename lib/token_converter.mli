type string_error = InvalidEscape of { seq : string; span : Source.span }
type convert_error = StringError of string_error list

val convert_token : Token.t -> Source.manager -> (Token.t, convert_error) result
