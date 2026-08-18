let%expect_test "ucn in suffix" =
  Test.test_converter {|
123\u0075
456\u
789\U00000076
|};
  [%expect {|
     4:14  Eof                     lexeme="\n"

    [1mconverter_test:2:4[0m: [1;31merror[0m: [1minvalid suffix '\u0075' on integer constant[0m
        2 | 123\u0075
          |    [1;32m^[0m[1;32m~~~~~[0m
    [1mconverter_test:3:4[0m: [1;31merror[0m: [1minvalid suffix '\u' on integer constant[0m
        3 | 456\u
          |    [1;32m^[0m[1;32m~[0m
    [1mconverter_test:4:4[0m: [1;31merror[0m: [1minvalid suffix '\U00000076' on integer constant[0m
        4 | 789\U00000076
          |    [1;32m^[0m[1;32m~~~~~~~~~[0m
    |}]
