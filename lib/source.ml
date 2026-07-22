type t = { filepath : string; contents : string }
type id = int
type span = { source_id : id; start : int; length : int }

module IntMap = Map.Make (Int)

type manager = { next_id : id; sources : t IntMap.t }

let create_source (filepath : string) (contents : string) =
  { filepath; contents }

let create_manager = { next_id = 0; sources = IntMap.empty }

let add_source (manager : manager) (source : t) : manager * id =
  ( {
      next_id = manager.next_id + 1;
      sources = IntMap.add manager.next_id source manager.sources;
    },
    manager.next_id )

let get_source (manager : manager) (id : id) : t =
  IntMap.find id manager.sources

let span_to_string (span : span) (manager : manager) : string =
  let source = get_source manager span.source_id in
  String.sub source.contents span.start span.length
