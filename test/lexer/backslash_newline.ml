let%expect_test "backslash newline" =
  Test.test_lexer ~verbose:true
    {|
#include <my\
header>

ident\
ifier

123\
.456

'a\
b'

"a\
b"

..\
.
|};
  [%expect
    {|
    Hash
      loc: 2:1
      lexeme: "#"

    Identifier("include")
      loc: 2:2
      lexeme: "include"

    HeaderName("myheader")
      loc: 2:10
      filepath: "myheader"
      type: NonLocal
      lexeme: "<my\\\nheader>"

    Identifier("identifier")
      loc: 5:1
      lexeme: "ident\\\nifier"

    PPNumber("123.456")
      loc: 8:1
      splices:
        { i:    0; loc:   8:1   }
        { i:    3; loc:   9:1   }
      lexeme: "123\\\n.456"

    PPChar("ab")
      loc: 11:1
      splices:
        { i:    0; loc:  11:2   }
        { i:    1; loc:  12:1   }
      lexeme: "'a\\\nb'"

    PPString("ab")
      loc: 14:1
      prefix: None
      splices:
        { i:    0; loc:  14:2   }
        { i:    1; loc:  15:1   }
      lexeme: "\"a\\\nb\""

    Ellipses
      loc: 17:1
      lexeme: "..\\\n."

    Eof
      loc: 18:2
      lexeme: "\n"
    |}]
