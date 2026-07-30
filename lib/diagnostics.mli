type loc = { line : int; col : int }

type t = {
  source : Source.t;
  highlight_start : loc;
  highlight_end : loc option;
  message : string;
}

val make_loc : int -> int -> loc
val at : Source.t -> loc -> string -> t
val range : Source.t -> loc -> loc -> string -> t
val emit_warning : t -> unit
val emit_error : t -> unit

val emit_fatal_error : t -> int -> 'a
(** [emit_fatal_error] prints the given diagnostic and then calls [exit] with
    the provided [exit_code].*)

val emit_driver_error : string -> unit
(** [emit_driver_error] is called for errors when invoking the toolchain.*)
