{
module Grammar where
import Tokens
}

%name parseCql
%tokentype { Token }
%error { parseError }

%token
  take    { TokenTake }
  from    { TokenFrom }
  read    { TokenRead }
  where   { TokenWhere }
  var     { TokenVar $$ }
  file    { TokenFile $$ }
  ','     { TokenComma }
  '='     { TokenEq }
  '^'     { TokenConjoin }
  '('     { TokenLParen }
  ')'     { TokenRParen }

%%

Exp : take Vars from Files       {TakeFrom $2 $4}

Vars : var ',' Vars   { $1 : $3 }
     | var            { [$1] }

Files : File '^' File  { Conjoin $1 $3 }
      | File           { $1 }

File : file '(' Vars ')'     {File $1 $3 }

{
parseError :: [Token] -> a
parseError tokens = error "Parse error"

data Exp = TakeFrom [String] File deriving Show
data File = File String [String] | Conjoin File File deriving Show


}
