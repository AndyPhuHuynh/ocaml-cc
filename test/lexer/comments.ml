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
    2:1   PPIdentifier(One)       lexeme="One"
    3:1   PPIdentifier(Two)       lexeme="Two"
    5:11  PPIdentifier(Three)     lexeme="Three"
    8:12  Eof                     lexeme="\n"
    |}]
