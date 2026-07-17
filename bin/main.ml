open Ocaml_cc

let read_entire_file name = In_channel.with_open_text name In_channel.input_all

let () =
  let str = read_entire_file "data/lexer.txt" in
  let tokens = Lexer.tokenize_all str in
  List.iter (fun tok -> print_endline (Token.to_string tok str)) tokens
