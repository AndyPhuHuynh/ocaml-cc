type t = {
  source : Source.t;
  highlight_start : Source.loc;
  highlight_end : Source.loc option;
  message : string;
}

exception Exit of int

(* diagnostic constructors *)
val at : Source.t -> Source.loc -> string -> t
val range : Source.t -> Source.loc -> Source.loc -> string -> t
val from_span : Source.t -> Source.span -> string -> t

(* emit functions *)
val emit_warning : t -> unit
val emit_error : t -> unit

val emit_fatal_error : t -> int -> 'a
(** [emit_fatal_error] prints the given diagnostic and then calls [exit] with
    the provided [exit_code].*)

val emit_driver_error : string -> unit
(** [emit_driver_error] is called for errors when invoking the toolchain.*)
