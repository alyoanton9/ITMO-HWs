{
module Parser where

import Lexer
import Grammar
}

%name       parse
%tokentype  {Token}
%error      {parseError}

%token VAR    {VarToken $$}
%token LAMBDA {LambdaToken}
%token DOT    {DotToken}
%token LEFT   {LeftParToken}
%token RIGHT  {RightParToken}

%%

expr
    : apply LAMBDA VAR DOT expr {Apply $1 (Lambda $3 $5)}
    | LAMBDA VAR DOT expr       {Lambda $2 $4}
    | apply                     {$1}

apply
    : apply atom                {Apply $1 $2}
    | atom                      {$1}

atom
    : LEFT expr RIGHT           {$2}
    | VAR                       {Var $1}

{
    parseError _ = error "Error while parsing"
}
