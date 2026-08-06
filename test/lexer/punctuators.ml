let%expect_test "punctuators" =
  Helpers.test_lexer
    {|
[](){}.->
++--&*+-~!
/%<<>><><=>===!=^|&&||
?:;...
=*=/=%=+=-=<<=>>=&=^=|=
,###
<::><%%>%:%:%:
|};
  [%expect
    {|
    NewLine
      loc: 1:1
      lexeme: "\n"

    LeftBracket
      loc: 2:1
      lexeme: "["

    RightBracket
      loc: 2:2
      lexeme: "]"

    LeftParen
      loc: 2:3
      lexeme: "("

    RightParen
      loc: 2:4
      lexeme: ")"

    LeftBrace
      loc: 2:5
      lexeme: "{"

    RightBrace
      loc: 2:6
      lexeme: "}"

    Period
      loc: 2:7
      lexeme: "."

    Arrow
      loc: 2:8
      lexeme: "->"

    NewLine
      loc: 2:10
      lexeme: "\n"

    PlusPlus
      loc: 3:1
      lexeme: "++"

    MinusMinus
      loc: 3:3
      lexeme: "--"

    And
      loc: 3:5
      lexeme: "&"

    Star
      loc: 3:6
      lexeme: "*"

    Plus
      loc: 3:7
      lexeme: "+"

    Minus
      loc: 3:8
      lexeme: "-"

    Tilde
      loc: 3:9
      lexeme: "~"

    Bang
      loc: 3:10
      lexeme: "!"

    NewLine
      loc: 3:11
      lexeme: "\n"

    Slash
      loc: 4:1
      lexeme: "/"

    Percent
      loc: 4:2
      lexeme: "%"

    LessLess
      loc: 4:3
      lexeme: "<<"

    GreaterGreater
      loc: 4:5
      lexeme: ">>"

    Less
      loc: 4:7
      lexeme: "<"

    Greater
      loc: 4:8
      lexeme: ">"

    LessEqual
      loc: 4:9
      lexeme: "<="

    GreaterEqual
      loc: 4:11
      lexeme: ">="

    EqualEqual
      loc: 4:13
      lexeme: "=="

    BangEqual
      loc: 4:15
      lexeme: "!="

    Caret
      loc: 4:17
      lexeme: "^"

    Or
      loc: 4:18
      lexeme: "|"

    AndAnd
      loc: 4:19
      lexeme: "&&"

    OrOr
      loc: 4:21
      lexeme: "||"

    NewLine
      loc: 4:23
      lexeme: "\n"

    Question
      loc: 5:1
      lexeme: "?"

    Colon
      loc: 5:2
      lexeme: ":"

    Semicolon
      loc: 5:3
      lexeme: ";"

    Ellipses
      loc: 5:4
      lexeme: "..."

    NewLine
      loc: 5:7
      lexeme: "\n"

    Equal
      loc: 6:1
      lexeme: "="

    StarEqual
      loc: 6:2
      lexeme: "*="

    SlashEqual
      loc: 6:4
      lexeme: "/="

    PercentEqual
      loc: 6:6
      lexeme: "%="

    PlusEqual
      loc: 6:8
      lexeme: "+="

    MinusEqual
      loc: 6:10
      lexeme: "-="

    LessLessEqual
      loc: 6:12
      lexeme: "<<="

    GreaterGreaterEqual
      loc: 6:15
      lexeme: ">>="

    AndEqual
      loc: 6:18
      lexeme: "&="

    CaretEqual
      loc: 6:20
      lexeme: "^="

    OrEqual
      loc: 6:22
      lexeme: "|="

    NewLine
      loc: 6:24
      lexeme: "\n"

    Comma
      loc: 7:1
      lexeme: ","

    HashHash
      loc: 7:2
      lexeme: "##"

    Hash
      loc: 7:4
      lexeme: "#"

    NewLine
      loc: 7:5
      lexeme: "\n"

    LeftBracket
      loc: 8:1
      lexeme: "<:"

    RightBracket
      loc: 8:3
      lexeme: ":>"

    LeftBrace
      loc: 8:5
      lexeme: "<%"

    RightBrace
      loc: 8:7
      lexeme: "%>"

    HashHash
      loc: 8:9
      lexeme: "%:%:"

    Hash
      loc: 8:13
      lexeme: "%:"

    NewLine
      loc: 8:15
      lexeme: "\n"

    Eof
      loc: 8:15
      lexeme: "\n"
    |}]
