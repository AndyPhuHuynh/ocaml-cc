let%expect_test "valid ucn four digits" =
  Test.test_converter ~escaped:false
    {|
'\u65E5\u672C\u8A9E'
"\u65E5\u672C\u8A9E"
|};
  [%expect
    {|
    2:1   CharLiteral(日本語)  lexeme='\u65E5\u672C\u8A9E'
    3:1   StringLiteral(日本語)  lexeme="\u65E5\u672C\u8A9E"
    3:21  Eof                     lexeme=
    |}]

let%expect_test "valid ucn eight digits" =
  Test.test_converter ~escaped:false
    {|
'\U000065E5\U0000672C\U00008A9E'
"\U000065E5\U0000672C\U00008A9E"
|};
  [%expect
    {|
    2:1   CharLiteral(日本語)  lexeme='\U000065E5\U0000672C\U00008A9E'
    3:1   StringLiteral(日本語)  lexeme="\U000065E5\U0000672C\U00008A9E"
    3:33  Eof                     lexeme=
    |}]

let%expect_test "invalid ucn four digits" =
  Test.test_converter {|
"\u0041"
"\uD800"
"\u41"
"\uhello"
|};
  [%expect
    {|
     2:1   StringLiteral("")       lexeme="\"\\u0041\""
     3:1   StringLiteral("")       lexeme="\"\\uD800\""
     4:1   StringLiteral("")       lexeme="\"\\u41\""
     5:1   StringLiteral("hello")  lexeme="\"\\uhello\""
     5:10  Eof                     lexeme="\n"

    [1mconverter_test:2:2[0m: [1;31merror[0m: [1mcharacter <U+0041> cannot be specified by a universal character name[0m
        2 | "\u0041"
          |  [1;32m^[0m[1;32m~~~~~[0m
    [1mconverter_test:3:2[0m: [1;31merror[0m: [1mcharacter <U+D800> cannot be specified by a universal character name[0m
        3 | "\uD800"
          |  [1;32m^[0m[1;32m~~~~~[0m
    [1mconverter_test:4:2[0m: [1;31merror[0m: [1mincomplete universal character name[0m
        4 | "\u41"
          |  [1;32m^[0m[1;32m~~~[0m
    [1mconverter_test:5:2[0m: [1;31merror[0m: [1m\u used with no following hex digits[0m
        5 | "\uhello"
          |  [1;32m^[0m[1;32m~[0m
    |}]

let%expect_test "invalid ucn eight digits" =
  Test.test_converter {|
"\U00000041"
"\U0000D800"
"\U00041"
"\Uhello"
|};
  [%expect
    {|
     2:1   StringLiteral("")       lexeme="\"\\U00000041\""
     3:1   StringLiteral("")       lexeme="\"\\U0000D800\""
     4:1   StringLiteral("")       lexeme="\"\\U00041\""
     5:1   StringLiteral("hello")  lexeme="\"\\Uhello\""
     5:10  Eof                     lexeme="\n"

    [1mconverter_test:2:2[0m: [1;31merror[0m: [1mcharacter <U+00000041> cannot be specified by a universal character name[0m
        2 | "\U00000041"
          |  [1;32m^[0m[1;32m~~~~~~~~~[0m
    [1mconverter_test:3:2[0m: [1;31merror[0m: [1mcharacter <U+0000D800> cannot be specified by a universal character name[0m
        3 | "\U0000D800"
          |  [1;32m^[0m[1;32m~~~~~~~~~[0m
    [1mconverter_test:4:2[0m: [1;31merror[0m: [1mincomplete universal character name[0m
        4 | "\U00041"
          |  [1;32m^[0m[1;32m~~~~~~[0m
    [1mconverter_test:5:2[0m: [1;31merror[0m: [1m\u used with no following hex digits[0m
        5 | "\Uhello"
          |  [1;32m^[0m[1;32m~[0m
    |}]
