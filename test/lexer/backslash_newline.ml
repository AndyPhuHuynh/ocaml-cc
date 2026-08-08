let%expect_test "backslash newline" =
  Helpers.test_lexer ~verbose:true
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
  [%expect {||}]
