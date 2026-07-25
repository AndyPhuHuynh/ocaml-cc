type t = { filepath : string; contents : string }
type id
type span = { source_id : id; start : int; length : int }
type manager

val make_absolute_path : string -> string
val empty_manager : manager

val load_file : manager -> string -> manager * id * t
(** [load_file] accepts the [manager] and an *absolute* [filepath]. *)

val get_source : manager -> id -> t
val span_to_string : span -> manager -> string
