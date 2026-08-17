let%expect_test "ordinary char literal" =
  Test.test_lexer {|
'A' 'Z'
'a' 'z'
'1' '9'
'_' '!'
|};
  [%expect
    {|
    2:1   PPChar("A")             lexeme="'A'"
    2:5   PPChar("Z")             lexeme="'Z'"
    3:1   PPChar("a")             lexeme="'a'"
    3:5   PPChar("z")             lexeme="'z'"
    4:1   PPChar("1")             lexeme="'1'"
    4:5   PPChar("9")             lexeme="'9'"
    5:1   PPChar("_")             lexeme="'_'"
    5:5   PPChar("!")             lexeme="'!'"
    5:8   Eof                     lexeme="\n"
    |}]

let%expect_test "escape sequences" =
  Test.test_lexer
    {|
'\a' '\b' '\e' '\f'
'\n' '\r' '\t' '\v'
'\'' '\"' '\?' '\\'
|};
  [%expect
    {|
    2:1   PPChar("\\a")           lexeme="'\\a'"
    2:6   PPChar("\\b")           lexeme="'\\b'"
    2:11  PPChar("\\e")           lexeme="'\\e'"
    2:16  PPChar("\\f")           lexeme="'\\f'"
    3:1   PPChar("\\n")           lexeme="'\\n'"
    3:6   PPChar("\\r")           lexeme="'\\r'"
    3:11  PPChar("\\t")           lexeme="'\\t'"
    3:16  PPChar("\\v")           lexeme="'\\v'"
    4:1   PPChar("\\'")           lexeme="'\\''"
    4:6   PPChar("\\\"")          lexeme="'\\\"'"
    4:11  PPChar("\\?")           lexeme="'\\?'"
    4:16  PPChar("\\\\")          lexeme="'\\\\'"
    4:20  Eof                     lexeme="\n"
    |}]

let%expect_test "octal sequences" =
  Test.test_lexer {|
'\0' '\7' '\123' '\777'
|};
  [%expect
    {|
    2:1   PPChar("\\0")           lexeme="'\\0'"
    2:6   PPChar("\\7")           lexeme="'\\7'"
    2:11  PPChar("\\123")         lexeme="'\\123'"
    2:18  PPChar("\\777")         lexeme="'\\777'"
    2:24  Eof                     lexeme="\n"
    |}]

let%expect_test "hex sequences" =
  Test.test_lexer {|
'\x00' '\x1b' '\x41' '\xff'
|};
  [%expect
    {|
    2:1   PPChar("\\x00")         lexeme="'\\x00'"
    2:8   PPChar("\\x1b")         lexeme="'\\x1b'"
    2:15  PPChar("\\x41")         lexeme="'\\x41'"
    2:22  PPChar("\\xff")         lexeme="'\\xff'"
    2:28  Eof                     lexeme="\n"
    |}]

let%expect_test "multichar" =
  Test.test_lexer {|
'abc' 'def' 'ghi' '   '
|};
  [%expect
    {|
    2:1   PPChar("abc")           lexeme="'abc'"
    2:7   PPChar("def")           lexeme="'def'"
    2:13  PPChar("ghi")           lexeme="'ghi'"
    2:19  PPChar("   ")           lexeme="'   '"
    2:24  Eof                     lexeme="\n"
    |}]

let%expect_test "errors" =
  Test.test_lexer {|
''
'a
|};
  [%expect
    {|
    2:1   EmptyCharLiteral        lexeme="''"
    3:1   UnterminatedCharLiteral
    3:1   Eof                     lexeme="'a\n"
    |}]

let%expect_test "new_line_splicing" =
  Test.test_lexer ~verbose:true {|
'a\ 
b\ 
c\ 
d'
|};
  [%expect
    {|
    PPChar("abcd")
      loc: 2:1
      splices:
        { i:    0; loc:   2:2   }
        { i:    1; loc:   3:1   }
        { i:    2; loc:   4:1   }
        { i:    3; loc:   5:1   }
      lexeme: "'a\\ \nb\\ \nc\\ \nd'"

    Eof
      loc: 5:3
      lexeme: "\n"

    [1mlex_test:2:3[0m: [1;35mwarning[0m: [1mbackslash and newline separated by whitespace[0m
        2 | 'a\
          |   [1;32m^[0m
    [1mlex_test:3:2[0m: [1;35mwarning[0m: [1mbackslash and newline separated by whitespace[0m
        3 | b\
          |  [1;32m^[0m
    [1mlex_test:4:2[0m: [1;35mwarning[0m: [1mbackslash and newline separated by whitespace[0m
        4 | c\
          |  [1;32m^[0m
    |}]
