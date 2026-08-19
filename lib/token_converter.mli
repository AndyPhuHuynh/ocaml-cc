type invalid_ucn = NoDigits | Incomplete | InvalidCodePoint of string

type invalid_escape_type =
  | SeqNormal
  | OctalTooLarge
  | HexNoDigits
  | HexTooLarge
  | Ucn of invalid_ucn

type identifier_error = { type_ : invalid_ucn; span : Source.span }

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
  | IdentifierError of identifier_error list
  | StringError of string_error list
  | PPNumberError of pp_number_error

type conversion_result =
  | Success of Token.t
  | Recovered of Token.t * conversion_error
  | Unrecoverable of conversion_error

val emit_string_error : Diagnostics.engine -> Source.t -> string_error -> unit

val emit_pp_number_error :
  Diagnostics.engine -> Source.t -> pp_number_error -> unit

val emit_conversion_error :
  Diagnostics.engine -> Source.t -> conversion_error -> unit

val convert_token : Token.t -> Source.manager -> conversion_result
