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
     2:1   PPIdentifier(auto)      lexeme="auto"
     3:1   PPIdentifier(break)     lexeme="break"
     4:1   PPIdentifier(case)      lexeme="case"
     5:1   PPIdentifier(char)      lexeme="char"
     6:1   PPIdentifier(const)     lexeme="const"
     7:1   PPIdentifier(continue)  lexeme="continue"
     8:1   PPIdentifier(default)   lexeme="default"
     9:1   PPIdentifier(do)        lexeme="do"
    10:1   PPIdentifier(double)    lexeme="double"
    11:1   PPIdentifier(else)      lexeme="else"
    12:1   PPIdentifier(enum)      lexeme="enum"
    13:1   PPIdentifier(extern)    lexeme="extern"
    14:1   PPIdentifier(float)     lexeme="float"
    15:1   PPIdentifier(for)       lexeme="for"
    16:1   PPIdentifier(goto)      lexeme="goto"
    17:1   PPIdentifier(if)        lexeme="if"
    18:1   PPIdentifier(inline)    lexeme="inline"
    19:1   PPIdentifier(int)       lexeme="int"
    20:1   PPIdentifier(long)      lexeme="long"
    21:1   PPIdentifier(register)  lexeme="register"
    22:1   PPIdentifier(restrict)  lexeme="restrict"
    23:1   PPIdentifier(return)    lexeme="return"
    24:1   PPIdentifier(short)     lexeme="short"
    25:1   PPIdentifier(signed)    lexeme="signed"
    26:1   PPIdentifier(sizeof)    lexeme="sizeof"
    27:1   PPIdentifier(static)    lexeme="static"
    28:1   PPIdentifier(struct)    lexeme="struct"
    29:1   PPIdentifier(switch)    lexeme="switch"
    30:1   PPIdentifier(typedef)   lexeme="typedef"
    31:1   PPIdentifier(union)     lexeme="union"
    32:1   PPIdentifier(unsigned)  lexeme="unsigned"
    33:1   PPIdentifier(void)      lexeme="void"
    34:1   PPIdentifier(volatile)  lexeme="volatile"
    35:1   PPIdentifier(while)     lexeme="while"
    36:1   PPIdentifier(_Bool)     lexeme="_Bool"
    37:1   PPIdentifier(_Complex)  lexeme="_Complex"
    38:1   PPIdentifier(_Imaginary)  lexeme="_Imaginary"
    38:11  Eof                     lexeme="\n"
    |}]
