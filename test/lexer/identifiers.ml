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
     2:1   Identifier(auto)        lexeme="auto"
     3:1   Identifier(break)       lexeme="break"
     4:1   Identifier(case)        lexeme="case"
     5:1   Identifier(char)        lexeme="char"
     6:1   Identifier(const)       lexeme="const"
     7:1   Identifier(continue)    lexeme="continue"
     8:1   Identifier(default)     lexeme="default"
     9:1   Identifier(do)          lexeme="do"
    10:1   Identifier(double)      lexeme="double"
    11:1   Identifier(else)        lexeme="else"
    12:1   Identifier(enum)        lexeme="enum"
    13:1   Identifier(extern)      lexeme="extern"
    14:1   Identifier(float)       lexeme="float"
    15:1   Identifier(for)         lexeme="for"
    16:1   Identifier(goto)        lexeme="goto"
    17:1   Identifier(if)          lexeme="if"
    18:1   Identifier(inline)      lexeme="inline"
    19:1   Identifier(int)         lexeme="int"
    20:1   Identifier(long)        lexeme="long"
    21:1   Identifier(register)    lexeme="register"
    22:1   Identifier(restrict)    lexeme="restrict"
    23:1   Identifier(return)      lexeme="return"
    24:1   Identifier(short)       lexeme="short"
    25:1   Identifier(signed)      lexeme="signed"
    26:1   Identifier(sizeof)      lexeme="sizeof"
    27:1   Identifier(static)      lexeme="static"
    28:1   Identifier(struct)      lexeme="struct"
    29:1   Identifier(switch)      lexeme="switch"
    30:1   Identifier(typedef)     lexeme="typedef"
    31:1   Identifier(union)       lexeme="union"
    32:1   Identifier(unsigned)    lexeme="unsigned"
    33:1   Identifier(void)        lexeme="void"
    34:1   Identifier(volatile)    lexeme="volatile"
    35:1   Identifier(while)       lexeme="while"
    36:1   Identifier(_Bool)       lexeme="_Bool"
    37:1   Identifier(_Complex)    lexeme="_Complex"
    38:1   Identifier(_Imaginary)  lexeme="_Imaginary"
    38:11  Eof                     lexeme="\n"
    |}]
