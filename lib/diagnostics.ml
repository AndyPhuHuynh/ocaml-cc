let emit_helper (type_ : string) (filepath : string) (line : int) (col : int)
    (msg : string) : unit =
  Printf.eprintf "%s:%d:%d: %s: %s\n" filepath line col type_ msg

let emit_warning = emit_helper "warning"
let emit_error = emit_helper "error"

let emit_fatal_error (filepath : string) (line : int) (col : int) (msg : string)
    (exit_code : int) : 'a =
  emit_helper "fatal error" filepath line col msg;
  exit exit_code
