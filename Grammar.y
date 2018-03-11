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
  ';'     { TokenEnd }
  '&'     { TokenAnd }
  ','     { TokenComma }
  '='     { TokenEq }
  '^'     { TokenConjoin }
  '('     { TokenLParen }
  ')'     { TokenRParen }

%left '^'
%%

Exp : take Vars from Files where Wheres ';'   {TakeFromWhere $2 $4 $6}

Vars : var ',' Vars   { $1 : $3 }
     | var            { [$1] }

Files : Files '^' File  { Conjoin $1 $3 }
      | File           { $1 }

File : file '(' Vars ')'     { File $1 $3 }

Wheres : Wheres '&' Where   { And $1 $3 }
       | Where              { $1 }

Where : var '=' var         { Eq ($1,$3) }

{
parseError :: [Token] -> a
parseError tokens = error "Parse error"

data Exp = TakeFromWhere [String] File Where deriving Show
data File = File String [String] | Conjoin File File deriving Show
data Where = Eq (String,String) | And Where Where deriving Show


}
