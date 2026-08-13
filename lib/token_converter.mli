type invalid_escape_type = SeqNormal | HexNoDigits | HexTooLarge

type string_error = {
  seq_type : invalid_escape_type;
  seq : string;
  span : Source.span;
}

type pp_number_error =
  | InvalidDigit of { digit : char; is_octal : bool; loc : Source.loc }
  | InvalidSuffix of { suffix : string; is_float : bool; span : Source.span }
  | ExponentNoDigits of { loc : Source.loc }
  | HexFloatNoExponent of { loc : Source.loc }
  | HexFloatNoSignificand of { loc : Source.loc }

type conversion_error =
  | StringError of string_error list
  | PPNumberError of pp_number_error

type conversion_result =
  | Success of Token.t
  | Recovered of Token.t * conversion_error
  | Unrecoverable of conversion_error

val print_string_error : Source.t -> string_error -> unit
val print_pp_number_error : Source.t -> pp_number_error -> unit
val print_conversion_error : Source.t -> conversion_error -> unit
val convert_token : Token.t -> Source.manager -> conversion_result
