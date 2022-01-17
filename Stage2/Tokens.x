{
module Tokens where
import SyntaxTree
import Data.Char (ord)
}

%wrapper "basic"

$digit = 0-9
$lowercase = [a-z]

tokens :-

  $white+         ;
  $digit+         { \s -> TokenInt (LeafVal (read s)) }
  \+              { \_ -> TokenPlus }
  \-              { \_ -> TokenMinus }
  \*              { \_ -> TokenTimes }
  \/              { \_ -> TokenDiv }
  \(              { \_ -> TokenLParen }
  \)              { \_ -> TokenRParen }
  \=              { \s -> TokenEquals }
  $lowercase      { \s -> TokenVar (LeafVar s (ord (head s) + 3999))}
  begin           { \_ -> TokenBegin}
  end             { \_ -> TokenEnd}
  read            { \_ -> TokenRead}
  write           { \_ -> TokenWrite}
  \;              { \_ -> TokenStmtEnd}

{

data Token = TokenInt SyntaxTree
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenEquals
           | TokenVar SyntaxTree
           | TokenBegin
           | TokenEnd
           | TokenRead
           | TokenWrite
           | TokenStmtEnd
           deriving (Show)

scanTokens = alexScanTokens

}