let emit_helper (type_ : string) (filepath : string) (line : int) (col : int)
    (msg : string) : unit =
  let loc = Printf.sprintf "%s:%d:%d" filepath line col in
  Printf.eprintf "%s: %s: %s\n" (Color.bold loc) type_ (Color.bold msg)

let emit_warning = emit_helper (Color.bold_magenta "warning")
let emit_error = emit_helper (Color.bold_red "error")

let emit_fatal_error (filepath : string) (line : int) (col : int) (msg : string)
    (exit_code : int) : 'a =
  emit_helper (Color.bold_red "fatal error") filepath line col msg;
  exit exit_code

let emit_driver_error (msg : string) : unit =
  Printf.eprintf "ocaml-cc: error: %s\n" msg
