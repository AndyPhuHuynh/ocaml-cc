let%expect_test "valid int literals" =
  Test.test_converter {|
10  255
0xA 0xFF
012 0377
|};
  [%expect
    {|
    2:1   IntLiteral(10)          suffix: None lexeme="10"
    2:5   IntLiteral(255)         suffix: None lexeme="255"
    3:1   IntLiteral(10)          suffix: None lexeme="0xA"
    3:5   IntLiteral(255)         suffix: None lexeme="0xFF"
    4:1   IntLiteral(10)          suffix: None lexeme="012"
    4:5   IntLiteral(255)         suffix: None lexeme="0377"
    4:9   Eof                     lexeme="\n"
    |}]

let%expect_test "valid int literal suffix" =
  Test.test_converter
    {|
123u   123U
123l   123L
123ll  123LL
123ul  123uL 123Ul 123UL
123lu  123lU 123Lu 123LU
123ull 123uLL 123Ull 123ULL
123llu 123llU 123LLu 123LLU
|};
  [%expect
    {|
    2:1   IntLiteral(123)         suffix: U    lexeme="123u"
    2:8   IntLiteral(123)         suffix: U    lexeme="123U"
    3:1   IntLiteral(123)         suffix: L    lexeme="123l"
    3:8   IntLiteral(123)         suffix: L    lexeme="123L"
    4:1   IntLiteral(123)         suffix: LL   lexeme="123ll"
    4:8   IntLiteral(123)         suffix: LL   lexeme="123LL"
    5:1   IntLiteral(123)         suffix: UL   lexeme="123ul"
    5:8   IntLiteral(123)         suffix: UL   lexeme="123uL"
    5:14  IntLiteral(123)         suffix: UL   lexeme="123Ul"
    5:20  IntLiteral(123)         suffix: UL   lexeme="123UL"
    6:1   IntLiteral(123)         suffix: UL   lexeme="123lu"
    6:8   IntLiteral(123)         suffix: UL   lexeme="123lU"
    6:14  IntLiteral(123)         suffix: UL   lexeme="123Lu"
    6:20  IntLiteral(123)         suffix: UL   lexeme="123LU"
    7:1   IntLiteral(123)         suffix: ULL  lexeme="123ull"
    7:8   IntLiteral(123)         suffix: ULL  lexeme="123uLL"
    7:15  IntLiteral(123)         suffix: ULL  lexeme="123Ull"
    7:22  IntLiteral(123)         suffix: ULL  lexeme="123ULL"
    8:1   IntLiteral(123)         suffix: ULL  lexeme="123llu"
    8:8   IntLiteral(123)         suffix: ULL  lexeme="123llU"
    8:15  IntLiteral(123)         suffix: ULL  lexeme="123LLu"
    8:22  IntLiteral(123)         suffix: ULL  lexeme="123LLU"
    8:28  Eof                     lexeme="\n"
    |}]

let%expect_test "invalid int literal suffix" =
  Test.test_converter {|
12345qwr_
0xABC_ghi
01234g.62

123abc
012789
012def
|};
  [%expect
    {|
     8:7   Eof                     lexeme="\n"

    [1mconverter_test:2:6[0m: [1;31merror[0m: [1minvalid suffix 'qwr_' on integer constant[0m
        2 | 12345qwr_
          |      [1;32m^[0m[1;32m~~~[0m
    [1mconverter_test:3:6[0m: [1;31merror[0m: [1minvalid suffix '_ghi' on integer constant[0m
        3 | 0xABC_ghi
          |      [1;32m^[0m[1;32m~~~[0m
    [1mconverter_test:4:6[0m: [1;31merror[0m: [1minvalid suffix 'g.62' on integer constant[0m
        4 | 01234g.62
          |      [1;32m^[0m[1;32m~~~[0m
    [1mconverter_test:6:4[0m: [1;31merror[0m: [1minvalid digit 'a' in integer constant[0m
        6 | 123abc
          |    [1;32m^[0m
    [1mconverter_test:7:5[0m: [1;31merror[0m: [1minvalid digit '8' in octal constant[0m
        7 | 012789
          |     [1;32m^[0m
    [1mconverter_test:8:4[0m: [1;31merror[0m: [1minvalid digit 'd' in octal constant[0m
        8 | 012def
          |    [1;32m^[0m
    |}]

let%expect_test "valid float literals" =
  Test.test_converter
    {|
1.2f 3.4F 
5.6l 7.8L

.12
.12e2
.12e+2
.12e-2

34e3
34e+3
34e-3
|};
  [%expect
    {|
     2:1   FloatLiteral(6/5)       suffix: F    lexeme="1.2f"
     2:6   FloatLiteral(17/5)      suffix: F    lexeme="3.4F"
     3:1   FloatLiteral(28/5)      suffix: L    lexeme="5.6l"
     3:6   FloatLiteral(39/5)      suffix: L    lexeme="7.8L"
     5:1   FloatLiteral(3/25)      suffix: None lexeme=".12"
     6:1   FloatLiteral(12)        suffix: None lexeme=".12e2"
    10:1   FloatLiteral(34000)     suffix: None lexeme="34e3"
    12:6   Eof                     lexeme="\n"

    [1mconverter_test:7:4[0m: [1;31merror[0m: [1mexponent has no digits[0m
        7 | .12e+2
          |    [1;32m^[0m
    [1mconverter_test:8:4[0m: [1;31merror[0m: [1mexponent has no digits[0m
        8 | .12e-2
          |    [1;32m^[0m
    [1mconverter_test:11:3[0m: [1;31merror[0m: [1mexponent has no digits[0m
       11 | 34e+3
          |   [1;32m^[0m
    [1mconverter_test:12:3[0m: [1;31merror[0m: [1mexponent has no digits[0m
       12 | 34e-3
          |   [1;32m^[0m
    |}]

let%expect_test "valid hexadecimal float literals" =
  Test.test_converter
    {|
0x.FFp0
0xFF.p0
0xFF.Ap0

0xFF.FFp2
0xFF.FFp+2

0xFF.FFP3
0xFF.FFP-3
|};
  [%expect
    {|
     2:1   FloatLiteral(255/256)   suffix: None lexeme="0x.FFp0"
     3:1   FloatLiteral(255)       suffix: None lexeme="0xFF.p0"
     4:1   FloatLiteral(2045/8)    suffix: None lexeme="0xFF.Ap0"
     6:1   FloatLiteral(65535/64)  suffix: None lexeme="0xFF.FFp2"
     7:1   FloatLiteral(65535/64)  suffix: None lexeme="0xFF.FFp+2"
     9:1   FloatLiteral(65535/32)  suffix: None lexeme="0xFF.FFP3"
    10:1   FloatLiteral(65535/2048)  suffix: None lexeme="0xFF.FFP-3"
    10:11  Eof                     lexeme="\n"
    |}]

let%expect_test "exponent no digits" =
  Test.test_converter
    {|
123e
123E
456e++1
456E--2

0xFFp
0xFFP
0xFFp++1
0xFFp--2
|};
  [%expect
    {|
     4:6   Plus                    lexeme="+"
     4:7   IntLiteral(1)           suffix: None lexeme="1"
     5:6   Minus                   lexeme="-"
     5:7   IntLiteral(2)           suffix: None lexeme="2"
     9:7   Plus                    lexeme="+"
     9:8   IntLiteral(1)           suffix: None lexeme="1"
    10:7   Minus                   lexeme="-"
    10:8   IntLiteral(2)           suffix: None lexeme="2"
    10:9   Eof                     lexeme="\n"

    [1mconverter_test:2:4[0m: [1;31merror[0m: [1mexponent has no digits[0m
        2 | 123e
          |    [1;32m^[0m
    [1mconverter_test:3:4[0m: [1;31merror[0m: [1mexponent has no digits[0m
        3 | 123E
          |    [1;32m^[0m
    [1mconverter_test:4:4[0m: [1;31merror[0m: [1mexponent has no digits[0m
        4 | 456e++1
          |    [1;32m^[0m
    [1mconverter_test:5:4[0m: [1;31merror[0m: [1mexponent has no digits[0m
        5 | 456E--2
          |    [1;32m^[0m
    [1mconverter_test:7:5[0m: [1;31merror[0m: [1mhexadecimal floating constant requires an exponent[0m
        7 | 0xFFp
          |     [1;32m^[0m
    [1mconverter_test:8:5[0m: [1;31merror[0m: [1mhexadecimal floating constant requires an exponent[0m
        8 | 0xFFP
          |     [1;32m^[0m
    [1mconverter_test:9:5[0m: [1;31merror[0m: [1mhexadecimal floating constant requires an exponent[0m
        9 | 0xFFp++1
          |     [1;32m^[0m
    [1mconverter_test:10:5[0m: [1;31merror[0m: [1mhexadecimal floating constant requires an exponent[0m
       10 | 0xFFp--2
          |     [1;32m^[0m
    |}]

let%expect_test "hexadecimal float no exponent and significand" =
  Test.test_converter {|
0x.
0x.f 0x.F
0x.l 0x.L
|};
  [%expect {|
     4:10  Eof                     lexeme="\n"

    [1mconverter_test:2:4[0m: [1;31merror[0m: [1mhexadecimal floating constant requires a significand[0m
        2 | 0x.
          |    [1;32m^[0m
    [1mconverter_test:3:5[0m: [1;31merror[0m: [1mhexadecimal floating constant requires an exponent[0m
        3 | 0x.f 0x.F
          |     [1;32m^[0m
    [1mconverter_test:3:5[0m: [1;31merror[0m: [1mhexadecimal floating constant requires an exponent[0m
        3 | 0x.f 0x.F
          |     [1;32m^[0m
    [1mconverter_test:4:4[0m: [1;31merror[0m: [1mhexadecimal floating constant requires a significand[0m
        4 | 0x.l 0x.L
          |    [1;32m^[0m
    [1mconverter_test:4:4[0m: [1;31merror[0m: [1mhexadecimal floating constant requires a significand[0m
        4 | 0x.l 0x.L
          |    [1;32m^[0m
    |}]
