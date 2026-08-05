type inspect_result = (Token.t list * Source.manager, Source.load_error) result

val lex_all : Source.load_type -> inspect_result
val pp_all : Source.load_type -> inspect_result
