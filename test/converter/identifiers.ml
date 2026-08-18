let%expect_test "identifiers" = Test.test_converter {|
abc
_123
_abc
|};
  [%expect {|
    2:1   Identifier(abc)         lexeme="abc"
    3:1   Identifier(_123)        lexeme="_123"
    4:1   Identifier(_abc)        lexeme="_abc"
    4:5   Eof                     lexeme="\n"
    |}]
