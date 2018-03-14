{
module Grammar where
import Tokens
}

%name parseCql
%tokentype { Token }
%error { parseError }

%token
  fetch   { TokenFetch _ }
  from    { TokenFrom _ }
  where   { TokenWhere _ }
  var     { TokenVar $$ _ }
  '"'     { TokenQuote _ }
  '!='    { TokenNotEq _ }
  '>='    { TokenGtEq _ }
  '>'     { TokenGt _ }
  '<='    { TokenLtEq _ }
  '<'     { TokenLt _ }
  ';'     { TokenEnd _ }
  '&'     { TokenAnd _ }
  ','     { TokenComma _ }
  '='     { TokenEq _ }
  '^'     { TokenConjoin _ }
  '('     { TokenLParen _ }
  ')'     { TokenRParen _ }

%%

Exp : fetch Vars from Files where Wheres ';'   {TakeFromWhere $2 $4 $6}
    | fetch Vars from Files ';'                {TakeFrom $2 $4 }

Vars : var ',' Vars   { $1 : $3 }
     | var            { [$1] }

Files : Files '^' File  { Conjoin $1 $3 }
      | File           { $1 }

File : var '(' Vars ')'     { File $1 $3 }

Wheres : Wheres '&' Where   { And $1 $3 }
       | Where              { $1 }

Where : var '=' var           { Eq ($1,$3) }
       | var '!=' var         { NotEq ($1,$3) }
       | var '>' var          { Gt ($1,$3) }
       | var '>=' var         { GtEq ($1,$3) }
       | var '<' var          { Lt ($1,$3) }
       | var '<=' var         { LtEq ($1,$3) }
       | var '=' '"' var '"'  { EqLit ($1,$4) }
       | var '>' '"' var '"'  { GtLit ($1,$4) }
       | var '>=' '"' var '"' { GtEqLit ($1,$4) }
       | var '<' '"' var '"'  { LtLit ($1,$4) }
       | var '<=' '"' var '"' { LtEqLit ($1,$4) }
       | var '!=' '"' var '"' { NotEqLit ($1,$4) }

{
parseError :: [Token] -> a
parseError tokens
 | tokens == [] = error ("Parse error, missing ;")
 | otherwise = error ("Parse error at " ++ errorMessage (tokenPosn (head tokens)))

errorMessage :: AlexPosn -> String
errorMessage (AlexPn x y z) = "Ln: " ++ show y ++ " Col: " ++ show z

data Exp = TakeFromWhere [String] File Where | TakeFrom [String] File deriving Show
data File = File String [String] | Conjoin File File deriving Show
data Where = Gt (String,String) | Lt (String,String) | NotEq (String,String) | Eq (String,String) 
 | And Where Where | EqLit (String,String) | GtLit (String,String) | LtLit (String,String) 
 | NotEqLit (String,String) | GtEq (String,String) | LtEq (String,String) 
 | GtEqLit (String,String) | LtEqLit (String,String) deriving Show

}
