{
module Parser where

import Lexer
import Grammar
}

%name       parse
%tokentype  {Token}
%error      {parseError}

%token IMPL  {ImplToken}
%token OR    {DisjToken}
%token AND   {ConjToken}
%token NOT   {NotToken}
%token VAR   {VarToken $$}
%token LEFT  {LeftParToken}
%token RIGHT {RightParToken}

%%

Expression
    : Disj                  {$1}
    | Disj IMPL Expression  {Bin Impl $1 $3}

Disj
    : Conj                  {$1}
    | Disj OR Conj          {Bin Disj $1 $3}

Conj
    : Not                   {$1}
    | Conj AND Not          {Bin Conj $1 $3}

Not
    : NOT Not               {Not $2}
    | VAR                   {Var $1}
    | LEFT Expression RIGHT {$2}

{
parseError _ = error "Error while parsing"
}
