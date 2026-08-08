let%expect_test "after include" =
  Test.test_lexer ~verbose:true {|
#include <stdio.h>
#include "stdlib.h"
|};
  [%expect
    {|
    NewLine
      loc: 1:1
      lexeme: "\n"

    Hash
      loc: 2:1
      lexeme: "#"

    Identifier("include")
      loc: 2:2
      lexeme: "include"

    HeaderName("stdio.h")
      loc: 2:10
      filepath: "stdio.h"
      type: NonLocal
      lexeme: "<stdio.h>"

    NewLine
      loc: 2:19
      lexeme: "\n"

    Hash
      loc: 3:1
      lexeme: "#"

    Identifier("include")
      loc: 3:2
      lexeme: "include"

    HeaderName("stdlib.h")
      loc: 3:10
      filepath: "stdlib.h"
      type: Local
      lexeme: "\"stdlib.h\""

    NewLine
      loc: 3:20
      lexeme: "\n"

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
    1:1   NewLine                 lexeme="\n"
    2:1   Less                    lexeme="<"
    2:2   Identifier("stdio")     lexeme="stdio"
    2:7   Period                  lexeme="."
    2:8   Identifier("h")         lexeme="h"
    2:9   Greater                 lexeme=">"
    2:10  NewLine                 lexeme="\n"
    3:1   PPString("stdlib.h")    lexeme="\"stdlib.h\""
    3:11  NewLine                 lexeme="\n"
    3:11  Eof                     lexeme="\n"
    |}]
