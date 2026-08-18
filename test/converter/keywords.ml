let%expect_test "keywords" =
  Test.test_converter
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
_Alignas
_Alignof
_Atomic
_Bool
_Complex
_Generic
_Imaginary
_Noreturn
_Static_assert
_Thread_local
|};
  [%expect {|
     2:1   Auto                    lexeme="auto"
     3:1   Break                   lexeme="break"
     4:1   Case                    lexeme="case"
     5:1   Char                    lexeme="char"
     6:1   Const                   lexeme="const"
     7:1   Continue                lexeme="continue"
     8:1   Default                 lexeme="default"
     9:1   Do                      lexeme="do"
    10:1   Double                  lexeme="double"
    11:1   Else                    lexeme="else"
    12:1   Enum                    lexeme="enum"
    13:1   Extern                  lexeme="extern"
    14:1   Float                   lexeme="float"
    15:1   For                     lexeme="for"
    16:1   Goto                    lexeme="goto"
    17:1   If                      lexeme="if"
    18:1   Inline                  lexeme="inline"
    19:1   Int                     lexeme="int"
    20:1   Long                    lexeme="long"
    21:1   Register                lexeme="register"
    22:1   Restrict                lexeme="restrict"
    23:1   Return                  lexeme="return"
    24:1   Short                   lexeme="short"
    25:1   Signed                  lexeme="signed"
    26:1   Sizeof                  lexeme="sizeof"
    27:1   Static                  lexeme="static"
    28:1   Struct                  lexeme="struct"
    29:1   Switch                  lexeme="switch"
    30:1   Typedef                 lexeme="typedef"
    31:1   Union                   lexeme="union"
    32:1   Unsigned                lexeme="unsigned"
    33:1   Void                    lexeme="void"
    34:1   Volatile                lexeme="volatile"
    35:1   While                   lexeme="while"
    36:1   Alignas                 lexeme="_Alignas"
    37:1   Alignof                 lexeme="_Alignof"
    38:1   Atomic                  lexeme="_Atomic"
    39:1   Bool                    lexeme="_Bool"
    40:1   Complex                 lexeme="_Complex"
    41:1   Generic                 lexeme="_Generic"
    42:1   Imaginary               lexeme="_Imaginary"
    43:1   NoReturn                lexeme="_Noreturn"
    44:1   StaticAssert            lexeme="_Static_assert"
    45:1   ThreadLocal             lexeme="_Thread_local"
    45:14  Eof                     lexeme="\n"
    |}]
