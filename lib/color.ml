let reset_str = "\x1b[0m"

let wrap (code : string) (s : string) : string =
  Printf.sprintf "%s%s%s" code s reset_str

let bold_str = "\x1b[1m"
let bold = wrap bold_str
let bold_red_str = "\x1b[1;31m"
let bold_red = wrap bold_red_str
let bold_magenta_str = "\x1b[1;35m"
let bold_magenta = wrap bold_magenta_str
