type inspect_result = (Token.t list * Source.manager, Source.load_error) result

val print_result : ?escaped:bool -> ?verbose:bool -> inspect_result -> unit
val lex_all : Source.load_file -> inspect_result
val pp_all : Source.load_file -> Source.manager -> inspect_result
val convert_all : Source.load_file -> Source.manager -> inspect_result
