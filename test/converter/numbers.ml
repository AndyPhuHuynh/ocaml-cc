let%expect_test "numbers" =
  Test.test_converter
    ~initial_src:{ name = "convert-numbers"; contents = {|
123
456
678
|} }
    [];
  [%expect {|
    2:1   IntLiteral(123)         lexeme="123"
    3:1   IntLiteral(456)         lexeme="456"
    4:1   IntLiteral(678)         lexeme="678"
    4:4   Eof                     lexeme="\n"
    |}]
