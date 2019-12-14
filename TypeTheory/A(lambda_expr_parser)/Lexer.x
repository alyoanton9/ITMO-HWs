{
module Lexer where
}

%wrapper "basic"

$digit  = 0-9
$letter = [a-z]
$apost  = '

tokens :-
    $white+                             ;
    $letter [$letter $digit $apost]*    {\s -> VarToken s}
    \\                                  {\_ -> LambdaToken}
    \.                                  {\_ -> DotToken}
    \(                                  {\_ -> LeftParToken}
    \)                                  {\_ -> RightParToken}

{
data Token = VarToken String | LambdaToken | DotToken
                | LeftParToken | RightParToken
    deriving Eq
}
