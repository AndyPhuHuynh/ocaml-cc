let%expect_test "valid int literals" =
  Test.test_converter
    ~initial_src:{ name = "test"; contents = {|
10  255
0xA 0xFF
012 0377
|} }
    [];
  [%expect
    {|
    2:1   IntLiteral(10)          lexeme="10"
    2:5   IntLiteral(255)         lexeme="255"
    3:1   IntLiteral(10)          lexeme="0xA"
    3:5   IntLiteral(255)         lexeme="0xFF"
    4:1   IntLiteral(10)          lexeme="012"
    4:5   IntLiteral(255)         lexeme="0377"
    4:9   Eof                     lexeme="\n"
    |}]

let%expect_test "invalid int literal suffix" =
  Test.test_converter
    ~initial_src:
      { name = "test"; contents = {|
12345abc
0xABC_ghi
01234g.62
|} }
    [];
  [%expect
    {|
     4:10  Eof                     lexeme="\n"

    [1mtest:2:6[0m: [1;31merror[0m: [1minvalid suffix[0m
        2 | 12345abc
          |      [1;32m^[0m[1;32m~~[0m
    [1mtest:3:6[0m: [1;31merror[0m: [1minvalid suffix[0m
        3 | 0xABC_ghi
          |      [1;32m^[0m[1;32m~~~[0m
    [1mtest:4:6[0m: [1;31merror[0m: [1minvalid suffix[0m
        4 | 01234g.62
          |      [1;32m^[0m[1;32m~~~[0m
    |}]
