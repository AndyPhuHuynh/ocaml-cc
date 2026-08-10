let%expect_test "valid int literals" =
  Test.test_converter {|
10  255
0xA 0xFF
012 0377
|};
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
  Test.test_converter {|
12345qwr_
0xABC_ghi
01234g.62

123abc
012789
012def
|};
  [%expect
    {|
     4:10  Eof                     lexeme="\n"

    [1mconverter_test:2:6[0m: [1;31merror[0m: [1minvalid digit 'a' in integer constant[0m
        2 | 12345abc
          |      [1;32m^[0m
    [1mconverter_test:3:6[0m: [1;31merror[0m: [1minvalid suffix '_ghi' on integer constant[0m
        3 | 0xABC_ghi
          |      [1;32m^[0m[1;32m~~~[0m
    [1mconverter_test:4:6[0m: [1;31merror[0m: [1minvalid suffix 'g.62' on integer constant[0m
        4 | 01234g.62
          |      [1;32m^[0m[1;32m~~~[0m
    |}]
