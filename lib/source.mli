type t = { filepath : string; contents : string }
type id
type span = { source_id : id; start : int; length : int }
type manager

val create_source : string -> string -> t
val create_manager : manager
val add_source : manager -> t -> manager * id
val get_source : manager -> id -> t
val span_to_string : span -> manager -> string
