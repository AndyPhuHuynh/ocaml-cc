type t = { filepath : string; contents : string; line_offsets : int array }
type id = int
type loc = { line : int; col : int }
type span = { source_id : id; start : int; length : int }

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

type manager = {
  next_id : id;
  from_id : t IntMap.t;
  from_filepath : id StringMap.t;
}

type load_error = FileNotFound | IOError of string

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

let make_absolute_path (path : string) =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let cannonize_if_exists (path : string) =
  if Sys.file_exists path then Unix.realpath path else path

let is_regular_file (filepath : string) : bool =
  Sys.file_exists filepath && not (Sys.is_directory filepath)

let empty_manager =
  { next_id = 0; from_id = IntMap.empty; from_filepath = StringMap.empty }

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

let load_file (manager : manager) (filepath : string) :
    (manager * id * t, load_error) result =
  let filepath = filepath |> make_absolute_path |> cannonize_if_exists in
  match StringMap.find_opt filepath manager.from_filepath with
  | Some id -> Ok (manager, id, IntMap.find id manager.from_id)
  | None when not (is_regular_file filepath) -> Error FileNotFound
  | None -> (
      try
        let contents = read_entire_file filepath in
        let source =
          { filepath; contents; line_offsets = calculate_line_offsets contents }
        in
        Ok
          ( {
              next_id = manager.next_id + 1;
              from_id = IntMap.add manager.next_id source manager.from_id;
              from_filepath =
                StringMap.add filepath manager.next_id manager.from_filepath;
            },
            manager.next_id,
            source )
      with Sys_error msg -> Error (IOError msg))

let get_source (manager : manager) (id : id) : t =
  IntMap.find id manager.from_id

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
