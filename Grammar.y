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
  vars    { [TokenVar $$] } -- this is meant to represent a list of vars [I don't know if this works]
  '=='    { TokenEq }
  '!='    { TokenNotEq }
  '('     { TokenLParen }
  ')'     { TokenRParen }

--do we need an int thing


 --what are presumably associations? For each operator add association direct (L/R)
 --Except they're aren't any operators here.
 --I think
 --I'm too tired for this

Exp :   take vars from var vars Exp { Take $2 $4 $5 }
    |   from var vars Exp           { From $2 $3 $4 }
    |   if var '==' vars            { If $2 $4 }
    |   if var '!=' null            { If $2}
    |                               {} --when Exp needs to equal nothing
