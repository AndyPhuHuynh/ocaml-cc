type t = {
  source : Source.t;
  highlight_start : Source.loc;
  highlight_end : Source.loc option;
  message : string;
}

type include_loc = { file : string; loc : Source.loc }

type engine = {
  mutable num_warnings : int;
  mutable num_errors : int;
  mutable include_stack_printed : bool;
  mutable include_stack : include_loc list;
}

exception Exit of int

let create_engine () : engine =
  {
    num_warnings = 0;
    num_errors = 0;
    include_stack_printed = false;
    include_stack = [];
  }

let add_include (engine : engine) (file : string) (loc : Source.loc) : unit =
  engine.include_stack_printed <- false;
  engine.include_stack <- { file; loc } :: engine.include_stack

let remove_include (engine : engine) : unit =
  engine.include_stack_printed <- false;
  match engine.include_stack with
  | [] -> failwith "Attempting to remove empty include stack"
  | x :: xs -> engine.include_stack <- xs

let at (source : Source.t) (start : Source.loc) (message : string) : t =
  { source; highlight_start = start; highlight_end = None; message }

let range (source : Source.t) (start : Source.loc) (end_ : Source.loc)
    (message : string) : t =
  { source; highlight_start = start; highlight_end = Some end_; message }

let from_span (source : Source.t) (span : Source.span) (message : string) : t =
  let start = Source.get_loc_from_pos source span.start in
  let end_ = Source.get_span_end source span in
  range source start end_ message

let emit_line (line_num : int) (line_num_padding : int) (line : string) : unit =
  Printf.eprintf "%*d | %s\n" line_num_padding line_num line

let emit_include_stack (engine : engine) : unit =
  List.iter
    (fun { file; loc } ->
      Printf.eprintf "In file included from %s:%d:%d\n" file loc.line loc.col)
    (List.rev engine.include_stack)

let emit_single_caret (source : Source.t) (loc : Source.loc) =
  let line = Source.get_line source loc.line in
  let line_num_padding = 5 in

  emit_line loc.line line_num_padding line;
  Printf.eprintf "%s | %s%s\n"
    (String.make line_num_padding ' ')
    (String.make (loc.col - 1) ' ')
    (Color.bold_green "^")

let emit_multi_caret (source : Source.t) (start_loc : Source.loc)
    (end_loc : Source.loc) =
  if not (Source.is_loc_after end_loc start_loc) then begin
    failwith
      (Printf.sprintf "Invalid highlight range: Start(%d:%d) End(%d:%d)"
         start_loc.line start_loc.col end_loc.line end_loc.col)
  end;

  if end_loc.line = start_loc.line then begin
    let line = Source.get_line source start_loc.line in
    let line_num_padding = 5 in

    emit_line start_loc.line line_num_padding line;
    Printf.eprintf "%s | %s%s%s\n"
      (String.make line_num_padding ' ')
      (String.make (start_loc.col - 1) ' ')
      (Color.bold_green "^")
      (Color.bold_green (String.make (end_loc.col - start_loc.col) '~'))
  end
  else begin
    let start_line = Source.get_line source start_loc.line in
    let line_num_padding = 5 in

    emit_line start_loc.line line_num_padding start_line;
    Printf.eprintf "%s | %s%s%s\n"
      (String.make line_num_padding ' ')
      (String.make (start_loc.col - 1) ' ')
      (Color.bold_green "^")
      (Color.bold_green
         (String.make (String.length start_line - start_loc.col) '~'));

    let print_middle_lines (line_num : int) : unit =
      if line_num >= end_loc.line then ()
      else begin
        let line = Source.get_line source line_num in
        emit_line line_num line_num_padding line;
        Printf.eprintf "%s | %s\n"
          (String.make line_num_padding ' ')
          (Color.bold_green (String.make (String.length line) '~'))
      end
    in

    print_middle_lines (start_loc.line + 1);

    let end_line = Source.get_line source end_loc.line in
    emit_line end_loc.line line_num_padding end_line;
    Printf.eprintf "%s | %s\n"
      (String.make line_num_padding ' ')
      (Color.bold_green (String.make end_loc.col '~'))
  end

let emit_diagnostic (engine : engine) (severity : string) (diag : t) : unit =
  if not engine.include_stack_printed then begin
    emit_include_stack engine;
    engine.include_stack_printed <- true
  end;

  let loc =
    Printf.sprintf "%s:%d:%d" diag.source.display_name diag.highlight_start.line
      diag.highlight_start.col
  in
  Printf.eprintf "%s: %s: %s\n" (Color.bold loc) severity
    (Color.bold diag.message);

  match diag.highlight_end with
  | None -> emit_single_caret diag.source diag.highlight_start
  | Some end_loc -> emit_multi_caret diag.source diag.highlight_start end_loc

let emit_warning (engine : engine) (diag : t) : unit =
  engine.num_warnings <- engine.num_warnings + 1;
  emit_diagnostic engine (Color.bold_magenta "warning") diag

let emit_error (engine : engine) (diag : t) : unit =
  engine.num_errors <- engine.num_errors + 1;
  emit_diagnostic engine (Color.bold_red "error") diag

let emit_fatal_error (engine : engine) (diag : t) (exit_code : int) : 'a =
  emit_diagnostic engine (Color.bold_red "fatal error") diag;
  raise (Exit exit_code)

let emit_driver_error (msg : string) : unit =
  Printf.eprintf "ocaml-cc: error: %s\n" msg
