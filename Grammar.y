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
  '=='    { TokenEq }
  '!='    { TokenNotEq }
  '('     { TokenLParen }
  ')'     { TokenRParen }


 --what are presumably associations? For each operator add association direct (L/R)
