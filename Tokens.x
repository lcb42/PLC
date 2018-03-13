{ 
module Tokens where 
}

%wrapper "posn"

$digit = 0-9                --digits
$alpha = [a-zA-Z]           --alphabetic characters
$upper = [A-Z]              --filenames

tokens :-
  $white+     ;
  "--".*      ;
  $upper                         { \p s -> TokenFile s p }
  TAKE                           { \p s -> TokenTake p }
  FROM                           { \p s -> TokenFrom p }
  WHERE                          { \p s -> TokenWhere p }
  \,                             { \p s -> TokenComma p }
  \;                             { \p s -> TokenEnd p }
  \^                             { \p s -> TokenConjoin p }
  \=                             { \p s -> TokenEq p }
  \&                             { \p s -> TokenAnd p }
  \(                             { \p s -> TokenLParen p }
  \)                             { \p s -> TokenRParen p } 
  $alpha [$digit]*               { \p s -> TokenVar s p }

{
--Each action has type :: String -> Token
-- The token type:

data Token =
  TokenTake AlexPosn        |
  TokenFrom AlexPosn        |
  TokenWhere AlexPosn       |
  TokenComma AlexPosn       |
  TokenEnd AlexPosn         |
  TokenConjoin AlexPosn     |
  TokenEq AlexPosn          |
  TokenAnd AlexPosn         |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenVar String AlexPosn  |
  TokenFile String AlexPosn
  deriving (Eq, Show)


tokenPosn :: Token -> AlexPosn
tokenPosn (TokenFile s p) = p
tokenPosn (TokenTake p) = p
tokenPosn (TokenFrom p) = p
tokenPosn (TokenWhere p) = p
tokenPosn (TokenComma p) = p
tokenPosn (TokenEnd p) = p
tokenPosn (TokenConjoin p) = p
tokenPosn (TokenEq p) = p
tokenPosn (TokenAnd p) = p
tokenPosn (TokenLParen p) = p
tokenPosn (TokenRParen p) = p
tokenPosn (TokenVar s p) = p

}
