{ 
module Tokens where 
}

%wrapper "posn"

$digit = 0-9                --digits
$alpha = [a-zA-Z]           --alphabetic characters

tokens :-
  $white+     ;
  "--".*      ;
  TAKE                           { \p s -> TokenTake p }
  FROM                           { \p s -> TokenFrom p }
  WHERE                          { \p s -> TokenWhere p }
  \"                             { \p s -> TokenQuote p }
  \!\=                           { \p s -> TokenNotEq p}
  \>                             { \p s -> TokenGt p}
  \<                             { \p s -> TokenLt p}
  \,                             { \p s -> TokenComma p }
  \;                             { \p s -> TokenEnd p }
  \^                             { \p s -> TokenConjoin p }
  \=                             { \p s -> TokenEq p }
  \&                             { \p s -> TokenAnd p }
  \(                             { \p s -> TokenLParen p }
  \)                             { \p s -> TokenRParen p } 
  [$alpha $digit \_ \’]+         { \p s -> TokenVar s p }

{
--Each action has type :: String -> Token
-- The token type:

data Token =
  TokenTake AlexPosn        |
  TokenFrom AlexPosn        |
  TokenWhere AlexPosn       |
  TokenQuote AlexPosn       |
  TokenNotEq AlexPosn       |
  TokenGt AlexPosn          |
  TokenLt AlexPosn          |
  TokenComma AlexPosn       |
  TokenEnd AlexPosn         |
  TokenConjoin AlexPosn     |
  TokenEq AlexPosn          |
  TokenAnd AlexPosn         |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenVar String AlexPosn  
  deriving (Eq, Show)


tokenPosn :: Token -> AlexPosn
tokenPosn (TokenTake p) = p
tokenPosn (TokenFrom p) = p
tokenPosn (TokenWhere p) = p
tokenPosn (TokenQuote p) = p
tokenPosn (TokenNotEq p) = p
tokenPosn (TokenGt p) = p
tokenPosn (TokenLt p) = p
tokenPosn (TokenComma p) = p
tokenPosn (TokenEnd p) = p
tokenPosn (TokenConjoin p) = p
tokenPosn (TokenEq p) = p
tokenPosn (TokenAnd p) = p
tokenPosn (TokenLParen p) = p
tokenPosn (TokenRParen p) = p
tokenPosn (TokenVar s p) = p

}
