type t = {
  source : Source.t;
  highlight_start : Source.loc;
  highlight_end : Source.loc option;
  message : string;
}

type engine

exception Exit of int

(* engine constructor *)
val create_engine : unit -> engine

(* diagnostic constructors *)
val at : Source.t -> Source.loc -> string -> t
val range : Source.t -> Source.loc -> Source.loc -> string -> t
val from_span : Source.t -> Source.span -> string -> t

(* include stack *)
val add_include : engine -> string -> Source.loc -> unit
val remove_include : engine -> unit

(* emit functions *)
val emit_warning : engine -> t -> unit
val emit_error : engine -> t -> unit

val emit_fatal_error : engine -> t -> int -> 'a
(** [emit_fatal_error] prints the given diagnostic and then raises an [Exit]
    exception with the provided [exit_code].*)

val emit_driver_error : string -> unit
(** [emit_driver_error] is called for errors when invoking the toolchain.*)
