let%expect_test "after include" =
  Test.test_lexer ~verbose:true {|
#include <stdio.h>
#include "stdlib.h"
|};
  [%expect
    {|
    Hash
      loc: 2:1
      lexeme: "#"

    PPIdentifier(include)
      loc: 2:2
      lexeme: "include"

    HeaderName(stdio.h)
      loc: 2:10
      filepath: stdio.h
      type: NonLocal
      lexeme: "<stdio.h>"

    Hash
      loc: 3:1
      lexeme: "#"

    PPIdentifier(include)
      loc: 3:2
      lexeme: "include"

    HeaderName(stdlib.h)
      loc: 3:10
      filepath: stdlib.h
      type: Local
      lexeme: "\"stdlib.h\""

    Eof
      loc: 3:20
      lexeme: "\n"
    |}]

let%expect_test "not after include" =
  Test.test_lexer {|
<stdio.h>
"stdlib.h"
|};
  [%expect
    {|
    2:1   Less                    lexeme="<"
    2:2   PPIdentifier(stdio)     lexeme="stdio"
    2:7   Period                  lexeme="."
    2:8   PPIdentifier(h)         lexeme="h"
    2:9   Greater                 lexeme=">"
    3:1   PPString("stdlib.h")    prefix: None  lexeme="\"stdlib.h\""
    3:11  Eof                     lexeme="\n"
    |}]
