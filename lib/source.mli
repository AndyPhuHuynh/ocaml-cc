type t = {
  display_name : string;
  filepath : string;
  contents : string;
  line_offsets : int array;
}

type id
type loc = { line : int; col : int }
type pos = { index : int; loc : loc }
type span = { source_id : id; start : int; length : int }
type string_pos = { index : int; loc : loc }
type string_src = { string : string; positions : string_pos list }
type manager
type load_file = { display_name : string option; filepath : string }
type load_error = FileNotFound of string | IOError of string

(* loc *)
val make_loc : int -> int -> loc
val is_loc_after : loc -> loc -> bool
val get_loc_from_pos : t -> int -> loc
val get_pos_from_loc : t -> loc -> int
val default_pos : pos

(* path *)
val make_absolute_path : string -> string
val is_regular_file : string -> bool

(**)
val empty_manager : manager

(* load source *)
val load_file : manager -> load_file -> (manager * id * t, load_error) result

(**)
val get_source : manager -> id -> t
val get_line : t -> int -> string

(* span *)
val get_span_end : t -> span -> loc
val span_to_string : span -> manager -> string

(* string *)
val string_index_to_source_pos : t -> int -> string_pos list -> int
val pp_string_pos : Format.formatter -> string_pos -> unit
val pp_string_pos_list : Format.formatter -> string_pos list -> unit
