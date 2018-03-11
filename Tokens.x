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
  READ                           { \s -> TokenRead }
  WHERE                          { \s -> TokenWhere }
  \,                             { \s -> TokenComma }
  \^                             { \s -> TokenConjoin }
  \=                             { \s -> TokenEq }
  \(                             { \s -> TokenLParen }
  \)                             { \s -> TokenRParen } 
  $alpha [$digit]*               { \s -> TokenVar s }

{
--Each action has type :: String -> Token
-- The token type:

data Token =
  TokenTake       |
  TokenFrom       |
  TokenRead       |
  TokenWhere      |
  TokenComma      |
  TokenConjoin    |
  TokenEq         |
  TokenLParen     |
  TokenRParen     |
  TokenVar String |
  TokenFile String
  deriving (Eq, Show)
}
