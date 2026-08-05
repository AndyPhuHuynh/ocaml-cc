let%expect_test "punctuators" =
  Helpers.test_lexer
    {|
[](){}.->
++--&*+-~!
/%<<>><><=>===!=^|&&||
?:;...
=*=/=%=+=-=<<=>>=&=^=|=
,###
|};
  [%expect {||}]
