let%expect_test "include" =
  Test.test_pp
    ~initial_src:
      {
        name = "main.c";
        contents =
          {|
#include "foo.h"
#include "bar.h"
#include "baz.h"

int main() {}
|};
      }
    [
      { name = "foo.h"; contents = {|
int foo();
|} };
      { name = "bar.h"; contents = {|
int bar();
|} };
      { name = "baz.h"; contents = {|
int baz();
|} };
    ];
  [%expect
    {|
    2:1   PPIdentifier(int)       lexeme="int"
    2:5   PPIdentifier(foo)       lexeme="foo"
    2:8   LeftParen               lexeme="("
    2:9   RightParen              lexeme=")"
    2:10  Semicolon               lexeme=";"
    2:1   PPIdentifier(int)       lexeme="int"
    2:5   PPIdentifier(bar)       lexeme="bar"
    2:8   LeftParen               lexeme="("
    2:9   RightParen              lexeme=")"
    2:10  Semicolon               lexeme=";"
    2:1   PPIdentifier(int)       lexeme="int"
    2:5   PPIdentifier(baz)       lexeme="baz"
    2:8   LeftParen               lexeme="("
    2:9   RightParen              lexeme=")"
    2:10  Semicolon               lexeme=";"
    6:1   PPIdentifier(int)       lexeme="int"
    6:5   PPIdentifier(main)      lexeme="main"
    6:9   LeftParen               lexeme="("
    6:10  RightParen              lexeme=")"
    6:12  LeftBrace               lexeme="{"
    6:13  RightBrace              lexeme="}"
    6:14  Eof                     lexeme="\n"
    |}]

let%expect_test "header not found" =
  Test.test_pp
    ~initial_src:{ name = "main.c"; contents = "#include <not_real.h>" }
    [];
  [%expect
    {|
    [1mmain.c:1:10[0m: [1;31mfatal error[0m: [1m'not_real.h' file not found[0m
        1 | #include <not_real.h>
          |          [1;32m^[0m[1;32m~~~~~~~~~~~[0m
    Exit called with code 1
    |}]
