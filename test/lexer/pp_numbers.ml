let%expect_test "digits" =
  Helpers.test_lexer {|
0 1 2 3 4 5 6 7 8 9
123 456 789
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPNumber("0")           lexeme="0"
    2:3   PPNumber("1")           lexeme="1"
    2:5   PPNumber("2")           lexeme="2"
    2:7   PPNumber("3")           lexeme="3"
    2:9   PPNumber("4")           lexeme="4"
    2:11  PPNumber("5")           lexeme="5"
    2:13  PPNumber("6")           lexeme="6"
    2:15  PPNumber("7")           lexeme="7"
    2:17  PPNumber("8")           lexeme="8"
    2:19  PPNumber("9")           lexeme="9"
    2:20  NewLine                 lexeme="\n"
    3:1   PPNumber("123")         lexeme="123"
    3:5   PPNumber("456")         lexeme="456"
    3:9   PPNumber("789")         lexeme="789"
    3:12  NewLine                 lexeme="\n"
    3:12  Eof                     lexeme="\n"
    |}]

let%expect_test "with dot" =
  Helpers.test_lexer {|
.123 .456 .678
123. 456. 789.
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPNumber(".123")        lexeme=".123"
    2:6   PPNumber(".456")        lexeme=".456"
    2:11  PPNumber(".678")        lexeme=".678"
    2:15  NewLine                 lexeme="\n"
    3:1   PPNumber("123.")        lexeme="123."
    3:6   PPNumber("456.")        lexeme="456."
    3:11  PPNumber("789.")        lexeme="789."
    3:15  NewLine                 lexeme="\n"
    3:15  Eof                     lexeme="\n"
    |}]

let%expect_test "exponent sign" =
  Helpers.test_lexer {|
123e+1 456E-2
123p+1 456P-2
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPNumber("123e+1")      lexeme="123e+1"
    2:8   PPNumber("456E-2")      lexeme="456E-2"
    2:14  NewLine                 lexeme="\n"
    3:1   PPNumber("123p+1")      lexeme="123p+1"
    3:8   PPNumber("456P-2")      lexeme="456P-2"
    3:14  NewLine                 lexeme="\n"
    3:14  Eof                     lexeme="\n"
    |}]

let%expect_test "suffix" =
  Helpers.test_lexer {|
123abc 567.def 789e+10ghi
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPNumber("123abc")      lexeme="123abc"
    2:8   PPNumber("567.def")     lexeme="567.def"
    2:16  PPNumber("789e+10ghi")  lexeme="789e+10ghi"
    2:26  NewLine                 lexeme="\n"
    2:26  Eof                     lexeme="\n"
    |}]

let%expect_test "not pp-number" =
  Helpers.test_lexer {|
123+1 456-2
+1    -2
.foo  .
a1
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPNumber("123")         lexeme="123"
    2:4   Plus                    lexeme="+"
    2:5   PPNumber("1")           lexeme="1"
    2:7   PPNumber("456")         lexeme="456"
    2:10  Minus                   lexeme="-"
    2:11  PPNumber("2")           lexeme="2"
    2:12  NewLine                 lexeme="\n"
    3:1   Plus                    lexeme="+"
    3:2   PPNumber("1")           lexeme="1"
    3:7   Minus                   lexeme="-"
    3:8   PPNumber("2")           lexeme="2"
    3:9   NewLine                 lexeme="\n"
    4:1   Period                  lexeme="."
    4:2   Identifier("foo")       lexeme="foo"
    4:7   Period                  lexeme="."
    4:8   NewLine                 lexeme="\n"
    5:1   Identifier("a1")        lexeme="a1"
    5:3   NewLine                 lexeme="\n"
    5:3   Eof                     lexeme="\n"
    |}]
