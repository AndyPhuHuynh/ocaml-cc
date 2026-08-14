let%expect_test "single char escape sequences" =
  Test.test_converter
    {|
'\a\b\e\f'
'\n\r\t\v'
"\a\b\e\f"
"\n\r\t\v"
'\'\"\?\\'
"\'\"\?\\"
|};
  [%expect
    {|
    2:1   CharLiteral("\007\b\027\012")  lexeme="'\\a\\b\\e\\f'"
    3:1   CharLiteral("\n\r\t\011")  lexeme="'\\n\\r\\t\\v'"
    4:1   StringLiteral("\007\b\027\012")  lexeme="\"\\a\\b\\e\\f\""
    5:1   StringLiteral("\n\r\t\011")  lexeme="\"\\n\\r\\t\\v\""
    6:1   CharLiteral("'\"?\\")   lexeme="'\\'\\\"\\?\\\\'"
    7:1   StringLiteral("'\"?\\")  lexeme="\"\\'\\\"\\?\\\\\""
    7:11  Eof                     lexeme="\n"
    |}]

let%expect_test "hex escape sequences" =
  Test.test_converter {|
'\x41\x42\x43'
"\x44\x45\x46"
|};
  [%expect
    {|
    2:1   CharLiteral("ABC")      lexeme="'\\x41\\x42\\x43'"
    3:1   StringLiteral("DEF")    lexeme="\"\\x44\\x45\\x46\""
    3:15  Eof                     lexeme="\n"
    |}]

let%expect_test "octal hex escape sequences" =
  Test.test_converter {|
'\101\102\103'
"\104\105\106"

'1017'
|};
  [%expect
    {|
    2:1   CharLiteral("ABC")      lexeme="'\\101\\102\\103'"
    3:1   StringLiteral("DEF")    lexeme="\"\\104\\105\\106\""
    5:1   CharLiteral("1017")     lexeme="'1017'"
    5:7   Eof                     lexeme="\n"
    |}]

let%expect_test "invalid escape sequences" =
  Test.test_converter {|
'\\
d'
'\x'
'\xghi'
'\777'
'\888'
|};
  [%expect {|
     2:1   CharLiteral("d")        lexeme="'\\\\\nd'"
     4:1   CharLiteral("x")        lexeme="'\\x'"
     5:1   CharLiteral("xghi")     lexeme="'\\xghi'"
     6:1   CharLiteral("777")      lexeme="'\\777'"
     7:1   CharLiteral("888")      lexeme="'\\888'"
     7:7   Eof                     lexeme="\n"

    [1mconverter_test:2:2[0m: [1;35mwarning[0m: [1munknown escape sequence '\d'[0m
        2 | '\\
          |  [1;32m^[0m[1;32m~[0m
        3 | d'
          | [1;32m~[0m
    [1mconverter_test:4:2[0m: [1;31merror[0m: [1m\x used with no following hex digits[0m
        4 | '\x'
          |  [1;32m^[0m[1;32m~[0m
    [1mconverter_test:5:2[0m: [1;31merror[0m: [1m\x used with no following hex digits[0m
        5 | '\xghi'
          |  [1;32m^[0m[1;32m~[0m
    [1mconverter_test:6:3[0m: [1;31merror[0m: [1moctal escape sequence out of range[0m
        6 | '\777'
          |   [1;32m^[0m[1;32m~~~[0m
    [1mconverter_test:7:2[0m: [1;35mwarning[0m: [1munknown escape sequence '\8'[0m
        7 | '\888'
          |  [1;32m^[0m[1;32m~[0m
    |}]
