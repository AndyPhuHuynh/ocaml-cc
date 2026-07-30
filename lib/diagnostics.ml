type loc = { line : int; col : int }

type t = {
  source : Source.t;
  highlight_start : loc;
  highlight_end : loc option;
  message : string;
}

let make_loc (line : int) (col : int) : loc = { line; col }

let at (source : Source.t) (start : loc) (message : string) : t =
  { source; highlight_start = start; highlight_end = None; message }

let range (source : Source.t) (start : loc) (end_ : loc) (message : string) : t
    =
  { source; highlight_start = start; highlight_end = Some end_; message }

let emit_helper (severity : string) (diag : t) : unit =
  let loc =
    Printf.sprintf "%s:%d:%d" diag.source.filepath diag.highlight_start.line
      diag.highlight_start.col
  in
  Printf.eprintf "%s: %s: %s\n" (Color.bold loc) severity
    (Color.bold diag.message);

  let line = Source.get_line diag.source diag.highlight_start.line in
  let line_num_padding = 5 in
  Printf.eprintf "%*d | %s\n" line_num_padding diag.highlight_start.line line;
  Printf.eprintf "%s |\n" (String.make line_num_padding ' ')

let emit_warning = emit_helper (Color.bold_magenta "warning")
let emit_error = emit_helper (Color.bold_red "error")

let emit_fatal_error (diag : t) (exit_code : int) : 'a =
  emit_helper (Color.bold_red "fatal error") diag;
  exit exit_code

let emit_driver_error (msg : string) : unit =
  Printf.eprintf "ocaml-cc: error: %s\n" msg
