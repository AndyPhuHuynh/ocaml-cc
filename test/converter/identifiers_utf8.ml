let%expect_test "identifiers with ucn" =
  Test.test_converter {|
\u65E5\u672C\u8A9E
\U000065E5\U0000672C\U00008A9E
|};
  [%expect {|
    2:1   Identifier(日本語)   lexeme="\\u65E5\\u672C\\u8A9E"
    3:1   Identifier(日本語)   lexeme="\\U000065E5\\U0000672C\\U00008A9E"
    3:31  Eof                     lexeme="\n"
    |}]

let%expect_test "invalid identifiers with ucn" =
  Test.test_converter {|
\u0041
\uD800
\u41
\uhello
|};
  [%expect {|
     5:8   Eof                     lexeme="\n"

    [1mconverter_test:2:1[0m: [1;31merror[0m: [1mcharacter <U+0041> cannot be specified by a universal character name[0m
        2 | \u0041
          | [1;32m^[0m[1;32m~~~~~[0m
    [1mconverter_test:3:1[0m: [1;31merror[0m: [1mcharacter <U+D800> cannot be specified by a universal character name[0m
        3 | \uD800
          | [1;32m^[0m[1;32m~~~~~[0m
    [1mconverter_test:4:1[0m: [1;31merror[0m: [1mincomplete universal character name[0m
        4 | \u41
          | [1;32m^[0m[1;32m~~~[0m
    [1mconverter_test:5:1[0m: [1;31merror[0m: [1m\u used with no following hex digits[0m
        5 | \uhello
          | [1;32m^[0m[1;32m~[0m
    |}]
