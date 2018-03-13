{
module Grammar where
import Tokens
}

%name parseCql
%tokentype { Token }
%error { parseError }

%token
  take    { TokenTake _ }
  from    { TokenFrom _ }
  where   { TokenWhere _ }
  var     { TokenVar $$ _ }
  file    { TokenFile $$ _ }
  ';'     { TokenEnd _ }
  '&'     { TokenAnd _ }
  ','     { TokenComma _ }
  '='     { TokenEq _ }
  '^'     { TokenConjoin _ }
  '('     { TokenLParen _ }
  ')'     { TokenRParen _ }

%left '^'
%%

Exp : take Vars from Files where Wheres ';'   {TakeFromWhere $2 $4 $6}
    | take Vars from Files ';'                {TakeFrom $2 $4 }

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
parseError tokens
 | tokens == [] = error ("Parse error, missing ;")
 | otherwise = error ("Parse error at " ++ errorMessage (tokenPosn (head tokens)))

errorMessage :: AlexPosn -> String
errorMessage (AlexPn x y z) = "Ln: " ++ show y ++ " Col: " ++ show z

data Exp = TakeFromWhere [String] File Where | TakeFrom [String] File deriving Show
data File = File String [String] | Conjoin File File deriving Show
data Where = Eq (String,String) | And Where Where deriving Show

}
