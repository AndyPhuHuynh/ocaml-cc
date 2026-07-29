type string_error

val convert_string : string -> string * string_error list
val convert_token : Token.t -> Token.t
