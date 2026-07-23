let emit_helper (type_ : string) (filepath : string) (line : int) (col : int)
    (msg : string) : unit =
  Printf.printf "%s:%d:%d: %s: %s\n" filepath line col type_ msg

let emit_warning = emit_helper "warning"
let emit_error = emit_helper "error"
