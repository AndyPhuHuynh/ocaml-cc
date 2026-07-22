open Ocaml_cc

let read_entire_file name = In_channel.with_open_text name In_channel.input_all

let () =
  let usage_msg = "ocaml-cc -i <input>" in
  let input_file = ref "" in
  let speclist = [ ("-i", Arg.Set_string input_file, "Input file") ] in

  Arg.parse speclist ignore usage_msg;
  if !input_file = "" then begin
    prerr_endline "error: no input files";
    exit 1
  end;

  (* let tokens = Preprocessor.Lexer.tokenize_all !input_file str in *)
  let tokens, source_manager = Preprocessor.tokenize_all !input_file in
  List.iter
    (fun tok -> print_endline (Token.to_string tok source_manager))
    tokens
