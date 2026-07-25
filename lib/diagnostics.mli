val emit_warning : string -> int -> int -> string -> unit
(** [emit_warning] accepts the [filepath], [line], [column], and [message]*)

val emit_error : string -> int -> int -> string -> unit
(** [emit_error] accepts the [filepath], [line], [column], and [message]*)

val emit_fatal_error : string -> int -> int -> string -> int -> 'a
(** [emit_fatal_error] accepts the [filepath], [line], [column], [message], and
    [exit_code]. [exit] will be called with the provided [exit_code]*)
