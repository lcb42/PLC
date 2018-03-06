{
  module Tokens (tokens) where
}

%wrapper "basic"

$digit = 0-9                --digits
$alpha = [a-zA-Z]           --alphabetic characters

tokens :-
  $white+     ;
  "--".*      ;
  TAKE        { \s -> TokenTake }
  FROM        { \s -> TokenFrom }
  IF          { \s -> TokenIf }
  IMPORT      { \s -> TokenImport }
  NULL        { \s -> TokenNull }
  \==         { \s -> TokenEq }
  \!=         { \s -> TokenNotEq }
  \(          { \s -> TokenLParen }
  \)          { \s -> TokenRParen }
  $digit+     { \s -> TokenInt (read s) }
  $alpha      { \s -> TokenVar s }

{
      --Each action has type :: String -> Token


-- The token type:

data Token =
  TokenTake   |
  TokenFrom   |
  TokenIf     |
  TokenImport |
  TokenNull   |
  TokenEq     |
  TokenNotEq  |
  TokenLParen |
  TokenRParen |
  TokenInt    |
  TokenVar
  deriving (Eq, Show)
}
