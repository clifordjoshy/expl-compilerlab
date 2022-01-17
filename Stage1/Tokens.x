{
module Tokens where
import Tree
}

%wrapper "basic"

$digit = 0-9

tokens :-

  $white+                       ;
  $digit+                       { \s -> TokenInt (Leaf (read s)) }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }

{

-- The token type:

data Token = TokenInt Tree
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           deriving (Show)

scanTokens = alexScanTokens

}