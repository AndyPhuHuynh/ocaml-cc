type t = { filepath : string; contents : string }
type id = int
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

let make_absolute_path (path : string) =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let cannonize_if_exists (path : string) =
  if Sys.file_exists path then Unix.realpath path else path

let is_regular_file (filepath : string) : bool =
  Sys.file_exists filepath && not (Sys.is_directory filepath)

let empty_manager =
  { next_id = 0; from_id = IntMap.empty; from_filepath = StringMap.empty }

let load_file (manager : manager) (filepath : string) :
    (manager * id * t, load_error) result =
  let filepath = filepath |> make_absolute_path |> cannonize_if_exists in
  match StringMap.find_opt filepath manager.from_filepath with
  | Some id -> Ok (manager, id, IntMap.find id manager.from_id)
  | None when not (is_regular_file filepath) -> Error FileNotFound
  | None -> (
      try
        let contents = read_entire_file filepath in
        let source = { filepath; contents } in
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

let span_to_string (span : span) (manager : manager) : string =
  let source = get_source manager span.source_id in
  String.sub source.contents span.start span.length
