let%expect_test "ordinary char literal" =
  Test.test_lexer {|
"A" "Z"
"a" "z"
"1" "9"
"_" "!"
|};
  [%expect
    {|
    2:1   PPString("A")           prefix: None  lexeme="\"A\""
    2:5   PPString("Z")           prefix: None  lexeme="\"Z\""
    3:1   PPString("a")           prefix: None  lexeme="\"a\""
    3:5   PPString("z")           prefix: None  lexeme="\"z\""
    4:1   PPString("1")           prefix: None  lexeme="\"1\""
    4:5   PPString("9")           prefix: None  lexeme="\"9\""
    5:1   PPString("_")           prefix: None  lexeme="\"_\""
    5:5   PPString("!")           prefix: None  lexeme="\"!\""
    5:8   Eof                     lexeme="\n"
    |}]

let%expect_test "escape sequences" =
  Test.test_lexer
    {|
"\a" "\b" "\e" "\f"
"\n" "\r" "\t" "\v"
"\'" "\"" "\?" "\\"
|};
  [%expect
    {|
    2:1   PPString("\\a")         prefix: None  lexeme="\"\\a\""
    2:6   PPString("\\b")         prefix: None  lexeme="\"\\b\""
    2:11  PPString("\\e")         prefix: None  lexeme="\"\\e\""
    2:16  PPString("\\f")         prefix: None  lexeme="\"\\f\""
    3:1   PPString("\\n")         prefix: None  lexeme="\"\\n\""
    3:6   PPString("\\r")         prefix: None  lexeme="\"\\r\""
    3:11  PPString("\\t")         prefix: None  lexeme="\"\\t\""
    3:16  PPString("\\v")         prefix: None  lexeme="\"\\v\""
    4:1   PPString("\\'")         prefix: None  lexeme="\"\\'\""
    4:6   PPString("\\\"")        prefix: None  lexeme="\"\\\"\""
    4:11  PPString("\\?")         prefix: None  lexeme="\"\\?\""
    4:16  PPString("\\\\")        prefix: None  lexeme="\"\\\\\""
    4:20  Eof                     lexeme="\n"
    |}]

let%expect_test "octal sequences" =
  Test.test_lexer {|
"\0" "\7" "\123" "\777"
|};
  [%expect
    {|
    2:1   PPString("\\0")         prefix: None  lexeme="\"\\0\""
    2:6   PPString("\\7")         prefix: None  lexeme="\"\\7\""
    2:11  PPString("\\123")       prefix: None  lexeme="\"\\123\""
    2:18  PPString("\\777")       prefix: None  lexeme="\"\\777\""
    2:24  Eof                     lexeme="\n"
    |}]

let%expect_test "hex sequences" =
  Test.test_lexer {|
"\x00" "\x1b" "\x41" "\xff"
|};
  [%expect
    {|
    2:1   PPString("\\x00")       prefix: None  lexeme="\"\\x00\""
    2:8   PPString("\\x1b")       prefix: None  lexeme="\"\\x1b\""
    2:15  PPString("\\x41")       prefix: None  lexeme="\"\\x41\""
    2:22  PPString("\\xff")       prefix: None  lexeme="\"\\xff\""
    2:28  Eof                     lexeme="\n"
    |}]

let%expect_test "multichar" =
  Test.test_lexer {|
"abc" "def" "ghi" "   "
|};
  [%expect
    {|
    2:1   PPString("abc")         prefix: None  lexeme="\"abc\""
    2:7   PPString("def")         prefix: None  lexeme="\"def\""
    2:13  PPString("ghi")         prefix: None  lexeme="\"ghi\""
    2:19  PPString("   ")         prefix: None  lexeme="\"   \""
    2:24  Eof                     lexeme="\n"
    |}]

let%expect_test "errors" =
  Test.test_lexer {|
"a
|};
  [%expect
    {|
    2:1   UnterminatedStringLiteral
    2:1   Eof                     lexeme="\"a\n"
    |}]

let%expect_test "new_line_splicing" =
  Test.test_lexer ~verbose:true {|
"a\ 
b\ 
c\ 
d"
|};
  [%expect
    {|
    PPString("abcd")
      loc: 2:1
      prefix: None
      splices:
        { i:    0; loc:   2:2   }
        { i:    1; loc:   3:1   }
        { i:    2; loc:   4:1   }
        { i:    3; loc:   5:1   }
      lexeme: "\"a\\ \nb\\ \nc\\ \nd\""

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

let%expect_test "string literal prefixes" =
  Test.test_lexer
    {|
"No prefix"
u8"Utf8 prefix"
u"Utf16 prefix"
U"Utf32 prefix"
L"Wchar prefix"
|};
  [%expect
    {|
    2:1   PPString("No prefix")   prefix: None  lexeme="\"No prefix\""
    3:1   PPString("Utf8 prefix")  prefix: Utf8  lexeme="u8\"Utf8 prefix\""
    4:1   PPString("Utf16 prefix")  prefix: Utf16 lexeme="u\"Utf16 prefix\""
    5:1   PPString("Utf32 prefix")  prefix: Utf32 lexeme="U\"Utf32 prefix\""
    6:1   PPString("Wchar prefix")  prefix: Wchar lexeme="L\"Wchar prefix\""
    6:16  Eof                     lexeme="\n"
    |}]
