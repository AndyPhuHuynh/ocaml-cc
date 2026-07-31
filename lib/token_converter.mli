type string_error = InvalidEscape of { start : int; finish : int }
type convert_error = StringError of string_error list

val convert_string : string -> (string, string_error list) result
val convert_token : Token.t -> (Token.t, convert_error) result
