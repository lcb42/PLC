{
module Grammar where
import Tokens
}

%name
%tokentype { Token }
%error { parseError }
%token
  take    { TokenTake }
  from    { TokenFrom }
  if      { TokenIf }
  import  { TokenImport }
  null    { TokenNull }
  var     { TokenVar $$ }
  '=='    { TokenEq }
  '!='    { TokenNotEq }
  '('     { TokenLParen }
  ')'     { TokenRParen }
  '['     { TokenLSP }
  ']'     { TokenRSP }

--do we need an int thing

Vars:   var
    |   var Vars
    |                                       --the empty set

Exp :   take '[' Vars ']' from var '[' Vars ']' Exp  { Take $2 $4 $6 $7 }
    |   from var '[' Vars ']' Exp            { From $2 $4 $6 }
    |   if var '==' Exp             { If $2 $4 }
    |   if var '!=' null            { If $2}
    |   '(' Exp ')'                 { $2 }
    |                                       --when Exp = nothing
