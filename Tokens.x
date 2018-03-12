{ 
module Tokens where 
}

%wrapper "basic"

$digit = 0-9                --digits
$alpha = [a-zA-Z]           --alphabetic characters
$upper = [A-Z]              --filenames

tokens :-
  $white+     ;
  "--".*      ;
  $upper                         { \s -> TokenFile s }
  TAKE                           { \s -> TokenTake }
  FROM                           { \s -> TokenFrom }
  WHERE                          { \s -> TokenWhere }
  \,                             { \s -> TokenComma }
  \;                             { \s -> TokenEnd }
  \^                             { \s -> TokenConjoin }
  \=                             { \s -> TokenEq }
  \&                             { \s -> TokenAnd }
  \(                             { \s -> TokenLParen }
  \)                             { \s -> TokenRParen } 
  $alpha [$digit]*               { \s -> TokenVar s }

{
--Each action has type :: String -> Token
-- The token type:

data Token =
  TokenTake       |
  TokenFrom       |
  TokenWhere      |
  TokenComma      |
  TokenEnd        |
  TokenConjoin    |
  TokenEq         |
  TokenAnd        |
  TokenLParen     |
  TokenRParen     |
  TokenVar String |
  TokenFile String
  deriving (Eq, Show)
}
