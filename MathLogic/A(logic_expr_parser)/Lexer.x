{
module Lexer where
}

%wrapper "basic"

$digit  = 0-9
$letter = [A-Z]
$apost  = '

tokens :-
    $white+                             ;
    "->"                                {\_ -> ImplToken}
    \|                                  {\_ -> DisjToken}
    &                                   {\_ -> ConjToken}
    !                                   {\_ -> NotToken}
    $letter [$letter $digit $apost]*    {\s -> VarToken s}
    \(                                  {\_ -> LeftParToken}
    \)                                  {\_ -> RightParToken}

{
data Token = ImplToken | DisjToken | ConjToken | NotToken |
             VarToken String | LeftParToken | RightParToken
    deriving Eq
}
