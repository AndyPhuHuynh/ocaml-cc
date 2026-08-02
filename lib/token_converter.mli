type invalid_escape_type = SeqNormal | HexNoDigits | HexTooLarge

type string_error = {
  seq_type : invalid_escape_type;
  seq : string;
  span : Source.span;
}

type conversion_error = StringError of string_error list

type conversion_result =
  | Success of Token.t
  | Recovered of Token.t * conversion_error
  | Unrecoverable of conversion_error

val convert_token : Token.t -> Source.manager -> conversion_result
