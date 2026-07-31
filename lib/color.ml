let reset_str = "\x1b[0m"

let wrap (code : string) (s : string) : string =
  Printf.sprintf "%s%s%s" code s reset_str

(* bold *)
let bold_str = "\x1b[1m"
let bold = wrap bold_str

(* red = 1 *)
let bold_red_str = "\x1b[1;31m"
let bold_red = wrap bold_red_str

(* green = 2 *)
let bold_green_str = "\x1b[1;32m"
let bold_green = wrap bold_green_str

(* magenta = 5 *)
let bold_magenta_str = "\x1b[1;35m"
let bold_magenta = wrap bold_magenta_str
