let%expect_test "identifiers" =
  Test.test_lexer
    {|
auto
break
case
char
const
continue
default
do
double
else
enum
extern
float
for
goto
if
inline
int
long
register
restrict
return
short
signed
sizeof
static
struct
switch
typedef
union
unsigned
void
volatile
while
_Bool
_Complex
_Imaginary
|};
  [%expect
    {|
     1:1   NewLine                 lexeme="\n"
     2:1   Identifier("auto")      lexeme="auto"
     2:5   NewLine                 lexeme="\n"
     3:1   Identifier("break")     lexeme="break"
     3:6   NewLine                 lexeme="\n"
     4:1   Identifier("case")      lexeme="case"
     4:5   NewLine                 lexeme="\n"
     5:1   Identifier("char")      lexeme="char"
     5:5   NewLine                 lexeme="\n"
     6:1   Identifier("const")     lexeme="const"
     6:6   NewLine                 lexeme="\n"
     7:1   Identifier("continue")  lexeme="continue"
     7:9   NewLine                 lexeme="\n"
     8:1   Identifier("default")   lexeme="default"
     8:8   NewLine                 lexeme="\n"
     9:1   Identifier("do")        lexeme="do"
     9:3   NewLine                 lexeme="\n"
    10:1   Identifier("double")    lexeme="double"
    10:7   NewLine                 lexeme="\n"
    11:1   Identifier("else")      lexeme="else"
    11:5   NewLine                 lexeme="\n"
    12:1   Identifier("enum")      lexeme="enum"
    12:5   NewLine                 lexeme="\n"
    13:1   Identifier("extern")    lexeme="extern"
    13:7   NewLine                 lexeme="\n"
    14:1   Identifier("float")     lexeme="float"
    14:6   NewLine                 lexeme="\n"
    15:1   Identifier("for")       lexeme="for"
    15:4   NewLine                 lexeme="\n"
    16:1   Identifier("goto")      lexeme="goto"
    16:5   NewLine                 lexeme="\n"
    17:1   Identifier("if")        lexeme="if"
    17:3   NewLine                 lexeme="\n"
    18:1   Identifier("inline")    lexeme="inline"
    18:7   NewLine                 lexeme="\n"
    19:1   Identifier("int")       lexeme="int"
    19:4   NewLine                 lexeme="\n"
    20:1   Identifier("long")      lexeme="long"
    20:5   NewLine                 lexeme="\n"
    21:1   Identifier("register")  lexeme="register"
    21:9   NewLine                 lexeme="\n"
    22:1   Identifier("restrict")  lexeme="restrict"
    22:9   NewLine                 lexeme="\n"
    23:1   Identifier("return")    lexeme="return"
    23:7   NewLine                 lexeme="\n"
    24:1   Identifier("short")     lexeme="short"
    24:6   NewLine                 lexeme="\n"
    25:1   Identifier("signed")    lexeme="signed"
    25:7   NewLine                 lexeme="\n"
    26:1   Identifier("sizeof")    lexeme="sizeof"
    26:7   NewLine                 lexeme="\n"
    27:1   Identifier("static")    lexeme="static"
    27:7   NewLine                 lexeme="\n"
    28:1   Identifier("struct")    lexeme="struct"
    28:7   NewLine                 lexeme="\n"
    29:1   Identifier("switch")    lexeme="switch"
    29:7   NewLine                 lexeme="\n"
    30:1   Identifier("typedef")   lexeme="typedef"
    30:8   NewLine                 lexeme="\n"
    31:1   Identifier("union")     lexeme="union"
    31:6   NewLine                 lexeme="\n"
    32:1   Identifier("unsigned")  lexeme="unsigned"
    32:9   NewLine                 lexeme="\n"
    33:1   Identifier("void")      lexeme="void"
    33:5   NewLine                 lexeme="\n"
    34:1   Identifier("volatile")  lexeme="volatile"
    34:9   NewLine                 lexeme="\n"
    35:1   Identifier("while")     lexeme="while"
    35:6   NewLine                 lexeme="\n"
    36:1   Identifier("_Bool")     lexeme="_Bool"
    36:6   NewLine                 lexeme="\n"
    37:1   Identifier("_Complex")  lexeme="_Complex"
    37:9   NewLine                 lexeme="\n"
    38:1   Identifier("_Imaginary")  lexeme="_Imaginary"
    38:11  NewLine                 lexeme="\n"
    38:11  Eof                     lexeme="\n"
    |}]
