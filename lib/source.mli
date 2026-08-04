type t = { filepath : string; contents : string; line_offsets : int array }
type id
type loc = { line : int; col : int }
type span = { source_id : id; start : int; length : int }
type string_pos = { index : int; pos : int }
type string_src = { string : string; positions : string_pos list }
type manager
type load_error = FileNotFound | IOError of string

(* loc *)
val make_loc : int -> int -> loc
val is_loc_after : loc -> loc -> bool
val get_loc_from_pos : t -> int -> loc

(* path *)
val make_absolute_path : string -> string
val is_regular_file : string -> bool

(**)
val empty_manager : manager

(* load source *)
val load_string : manager -> name:string -> string -> manager * id * t
val load_file : manager -> string -> (manager * id * t, load_error) result

(**)
val get_source : manager -> id -> t
val get_line : t -> int -> string

(* span *)
val get_span_end : t -> span -> loc
val span_to_string : span -> manager -> string

(* string *)
val string_index_to_source_pos : int -> string_pos list -> int
