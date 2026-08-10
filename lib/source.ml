type t = {
  display_name : string;
  filepath : string;
  contents : string;
  line_offsets : int array;
}

type id = int
type loc = { line : int; col : int }
type pos = { index : int; loc : loc }
type span = { source_id : id; start : int; length : int }
type string_pos = { index : int; loc : loc }
type string_src = { string : string; positions : string_pos list }

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

type manager = {
  next_id : id;
  from_id : t IntMap.t;
  from_filepath : id StringMap.t;
}

type load_string = { name : string; contents : string }
type load_file = { display_name : string option; filepath : string }
type load_type = LoadString of load_string | LoadFile of load_file
type load_error = FileNotFound of string | IOError of string

let read_entire_file (name : string) : string =
  In_channel.with_open_text name In_channel.input_all

let calculate_line_offsets (contents : string) : int array =
  let len = String.length contents in
  let rec helper (pos : int) (acc : int list) : int array =
    if pos >= len then Array.of_list (List.rev acc)
    else
      match contents.[pos] with
      | '\n' ->
          if pos + 1 < len then helper (pos + 1) ((pos + 1) :: acc)
          else Array.of_list (List.rev acc)
      | _ -> helper (pos + 1) acc
  in

  if contents = "" then [||] else helper 0 [ 0 ]

let make_loc (line : int) (col : int) : loc = { line; col }

let is_loc_after (loc1 : loc) (loc2 : loc) : bool =
  loc1.line > loc2.line || (loc1.line = loc2.line && loc1.col > loc2.col)

let get_loc_from_pos (source : t) (pos : int) : loc =
  let rec loop (lo : int) (hi : int) : int =
    if lo > hi then hi
    else
      let mid = lo + ((hi - lo) / 2) in
      if source.line_offsets.(mid) <= pos then loop (mid + 1) hi
      else loop lo (mid - 1)
  in

  let index = loop 0 (Array.length source.line_offsets - 1) in
  let line = index + 1 in
  let col = pos - source.line_offsets.(index) + 1 in
  { line; col }

let get_pos_from_loc (source : t) (loc : loc) : int =
  source.line_offsets.(loc.line - 1) + (loc.col - 1)

let default_pos : pos = { index = 0; loc = { line = 1; col = 1 } }

let make_absolute_path (path : string) =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let cannonize_if_exists (path : string) =
  if Sys.file_exists path then Unix.realpath path else path

let is_regular_file (filepath : string) : bool =
  Sys.file_exists filepath && not (Sys.is_directory filepath)

let empty_manager =
  { next_id = 0; from_id = IntMap.empty; from_filepath = StringMap.empty }

let add_source (manager : manager) (source : t) : manager * id * t =
  let new_manager =
    {
      next_id = manager.next_id + 1;
      from_id = IntMap.add manager.next_id source manager.from_id;
      from_filepath =
        StringMap.add source.filepath manager.next_id manager.from_filepath;
    }
  in
  (new_manager, manager.next_id, source)

let load_string (manager : manager) (string : load_string) : manager * id * t =
  match StringMap.find_opt string.name manager.from_filepath with
  | Some id -> (manager, id, IntMap.find id manager.from_id)
  | None ->
      let source =
        {
          display_name = string.name;
          filepath = string.name;
          contents = string.contents;
          line_offsets = calculate_line_offsets string.contents;
        }
      in
      add_source manager source

let load_file (manager : manager) (file : load_file) :
    (manager * id * t, load_error) result =
  let filepath = file.filepath |> make_absolute_path |> cannonize_if_exists in
  match StringMap.find_opt filepath manager.from_filepath with
  | Some id -> Ok (manager, id, IntMap.find id manager.from_id)
  | None when not (is_regular_file filepath) ->
      let name = Option.value file.display_name ~default:file.filepath in
      Error (FileNotFound name)
  | None -> (
      try
        let display_name =
          match file.display_name with
          | None -> file.filepath
          | Some name -> name
        in
        let contents = read_entire_file filepath in
        let source =
          {
            display_name;
            filepath;
            contents;
            line_offsets = calculate_line_offsets contents;
          }
        in
        Ok (add_source manager source)
      with Sys_error msg -> Error (IOError msg))

let load (manager : manager) (type_ : load_type) :
    (manager * id * t, load_error) result =
  match type_ with
  | LoadString str -> Ok (load_string manager str)
  | LoadFile file -> load_file manager file

let get_source (manager : manager) (id : id) : t =
  IntMap.find id manager.from_id

let get_line (source : t) (line : int) : string =
  let line_offset = source.line_offsets.(line - 1) in
  let line_len =
    if line < Array.length source.line_offsets then
      source.line_offsets.(line) - source.line_offsets.(line - 1) - 1
    else String.length source.contents - source.line_offsets.(line - 1)
  in

  let rec trim_new_line (len : int) =
    if len <= 0 then 0
    else
      let c = source.contents.[line_offset + len - 1] in
      match c with '\r' | '\n' -> trim_new_line (len - 1) | _ -> len
  in

  let line_len = trim_new_line line_len in
  String.sub source.contents line_offset line_len

let get_line_from_pos (offsets : int array) (pos : int) : int =
  let rec loop (lo : int) (hi : int) : int =
    if lo > hi then hi
    else
      let mid = lo + ((hi - lo) / 2) in
      if offsets.(mid) <= pos then loop (mid + 1) hi else loop lo (mid - 1)
  in

  loop 0 (Array.length offsets - 1) + 1

let get_span_end (source : t) (span : span) : loc =
  let total_pos = span.start + span.length - 1 in
  let line = get_line_from_pos source.line_offsets total_pos in
  let offset = source.line_offsets.(line - 1) in
  let col = total_pos - offset + 1 in
  { line; col }

let span_to_string (span : span) (manager : manager) : string =
  let source = get_source manager span.source_id in
  String.sub source.contents span.start span.length

let string_index_to_source_pos (source : t) (index : int)
    (positions : string_pos list) : int =
  let rec find_base_index_pos (positions : string_pos list) : string_pos =
    match positions with
    | [] -> failwith "index_to_source_pos failed with empty positions"
    | [ x ] -> x
    | a :: b :: xs ->
        if b.index > index then a else find_base_index_pos (b :: xs)
  in

  let base_index_pos = find_base_index_pos positions in
  let base_pos = get_pos_from_loc source base_index_pos.loc in
  let offset = index - base_index_pos.index in

  base_pos + offset

let pp_string_pos (fmt : Format.formatter) (value : string_pos) : unit =
  Format.fprintf fmt "{ i: %4d; loc: %3d:%-3d }" value.index value.loc.line
    value.loc.col

let pp_string_pos_list (fmt : Format.formatter) (values : string_pos list) :
    unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
    pp_string_pos fmt values
