let%expect_test "ordinary char literal" =
  Helpers.test_lexer {|
"A" "Z"
"a" "z"
"1" "9"
"_" "!"
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPString("A")           lexeme="\"A\""
    2:5   PPString("Z")           lexeme="\"Z\""
    2:8   NewLine                 lexeme="\n"
    3:1   PPString("a")           lexeme="\"a\""
    3:5   PPString("z")           lexeme="\"z\""
    3:8   NewLine                 lexeme="\n"
    4:1   PPString("1")           lexeme="\"1\""
    4:5   PPString("9")           lexeme="\"9\""
    4:8   NewLine                 lexeme="\n"
    5:1   PPString("_")           lexeme="\"_\""
    5:5   PPString("!")           lexeme="\"!\""
    5:8   NewLine                 lexeme="\n"
    5:8   Eof                     lexeme="\n"
    |}]

let%expect_test "escape sequences" =
  Helpers.test_lexer
    {|
"\a" "\b" "\e" "\f"
"\n" "\r" "\t" "\v"
"\'" "\"" "\?" "\\"
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPString("\\a")         lexeme="\"\\a\""
    2:6   PPString("\\b")         lexeme="\"\\b\""
    2:11  PPString("\\e")         lexeme="\"\\e\""
    2:16  PPString("\\f")         lexeme="\"\\f\""
    2:20  NewLine                 lexeme="\n"
    3:1   PPString("\\n")         lexeme="\"\\n\""
    3:6   PPString("\\r")         lexeme="\"\\r\""
    3:11  PPString("\\t")         lexeme="\"\\t\""
    3:16  PPString("\\v")         lexeme="\"\\v\""
    3:20  NewLine                 lexeme="\n"
    4:1   PPString("\\'")         lexeme="\"\\'\""
    4:6   PPString("\\\"")        lexeme="\"\\\"\""
    4:11  PPString("\\?")         lexeme="\"\\?\""
    4:16  PPString("\\\\")        lexeme="\"\\\\\""
    4:20  NewLine                 lexeme="\n"
    4:20  Eof                     lexeme="\n"
    |}]

let%expect_test "octal sequences" =
  Helpers.test_lexer {|
"\0" "\7" "\123" "\777"
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPString("\\0")         lexeme="\"\\0\""
    2:6   PPString("\\7")         lexeme="\"\\7\""
    2:11  PPString("\\123")       lexeme="\"\\123\""
    2:18  PPString("\\777")       lexeme="\"\\777\""
    2:24  NewLine                 lexeme="\n"
    2:24  Eof                     lexeme="\n"
    |}]

let%expect_test "hex sequences" =
  Helpers.test_lexer {|
"\x00" "\x1b" "\x41" "\xff"
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPString("\\x00")       lexeme="\"\\x00\""
    2:8   PPString("\\x1b")       lexeme="\"\\x1b\""
    2:15  PPString("\\x41")       lexeme="\"\\x41\""
    2:22  PPString("\\xff")       lexeme="\"\\xff\""
    2:28  NewLine                 lexeme="\n"
    2:28  Eof                     lexeme="\n"
    |}]

let%expect_test "multichar" =
  Helpers.test_lexer {|
"abc" "def" "ghi" "   "
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   PPString("abc")         lexeme="\"abc\""
    2:7   PPString("def")         lexeme="\"def\""
    2:13  PPString("ghi")         lexeme="\"ghi\""
    2:19  PPString("   ")         lexeme="\"   \""
    2:24  NewLine                 lexeme="\n"
    2:24  Eof                     lexeme="\n"
    |}]

let%expect_test "errors" =
  Helpers.test_lexer {|
"a
|};
  [%expect
    {|
    1:1   NewLine                 lexeme="\n"
    2:1   UnterminatedStringLiteral
    2:1   Eof                     lexeme="\"a\n"
    |}]

let%expect_test "new_line_splicing" =
  Helpers.test_lexer ~verbose:true {|
"a\ 
b\ 
c\ 
d"
|};
  [%expect
    {|
    NewLine
      loc: 1:1
      lexeme: "\n"

    PPString("abcd")
      loc: 2:1
      splices:
        { i:    0; loc:   2:2   }
        { i:    1; loc:   3:1   }
        { i:    2; loc:   4:1   }
        { i:    3; loc:   5:1   }
      lexeme: "\"a\\ \nb\\ \nc\\ \nd\""

    NewLine
      loc: 5:3
      lexeme: "\n"

    Eof
      loc: 5:3
      lexeme: "\n"

    [1mlex_test:2:3[0m: [1;35mwarning[0m: [1mbackslash and newline separated by whitespace[0m
        2 | "a\
          |   [1;32m^[0m
    [1mlex_test:3:2[0m: [1;35mwarning[0m: [1mbackslash and newline separated by whitespace[0m
        3 | b\
          |  [1;32m^[0m
    [1mlex_test:4:2[0m: [1;35mwarning[0m: [1mbackslash and newline separated by whitespace[0m
        4 | c\
          |  [1;32m^[0m
    |}]
