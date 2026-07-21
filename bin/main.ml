open Ocaml_cc

let read_entire_file name = In_channel.with_open_text name In_channel.input_all

let () =
  let filename = "data/lexer_tests/backslash-newline.txt" in
  let str = read_entire_file filename in
  let tokens = Preprocessor.Lexer.tokenize_all filename str in
  List.iter (fun tok -> print_endline (Token.to_string tok str)) tokens
