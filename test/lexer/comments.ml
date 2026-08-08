let%expect_test "comments" =
  Test.test_lexer
    {|
One // This is a comment
Two /* This
is a multi-line
comment*/ Three 

/* // Nested comment */
/* /* // */
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   Identifier("One")       lexeme="One"
    3:1   Identifier("Two")       lexeme="Two"
    5:11  Identifier("Three")     lexeme="Three"
    5:17  NewLine                 lexeme="\n"
    6:1   NewLine                 lexeme="\n"
    7:24  NewLine                 lexeme="\n"
    8:12  NewLine                 lexeme="\n"
    8:12  Eof                     lexeme="\n"
    |}]
