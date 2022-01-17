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
  \=              { \_ -> TokenAssign }
  \<              { \_ -> TokenLt }
  \>              { \_ -> TokenGt }
  \<\=            { \_ -> TokenLtE }
  \>\=            { \_ -> TokenGtE }
  \!\=            { \_ -> TokenNE }
  \=\=            { \_ -> TokenEq }
  $lowercase      { \s -> TokenVar (LeafVar s (ord (head s) + 3999)) }
  begin           { \_ -> TokenBegin }
  end             { \_ -> TokenEnd }
  read            { \_ -> TokenRead }
  write           { \_ -> TokenWrite }
  if              { \_ -> TokenIf }
  then            { \_ -> TokenThen }
  else            { \_ -> TokenElse }
  endif           { \_ -> TokenEndif }
  while           { \_ -> TokenWhile }
  do              { \_ -> TokenDo }
  break           { \_ -> TokenBreak }
  continue        { \_ -> TokenCont }
  endwhile        { \_ -> TokenEndwhile }
  \;              { \_ -> TokenStmtEnd }

{

data Token = TokenInt SyntaxTree
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLt
           | TokenLtE
           | TokenGt
           | TokenGtE
           | TokenEq
           | TokenNE
           | TokenLParen
           | TokenRParen
           | TokenAssign
           | TokenVar SyntaxTree
           | TokenBegin
           | TokenEnd
           | TokenRead
           | TokenWrite
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenEndif
           | TokenWhile
           | TokenDo
           | TokenBreak
           | TokenCont
           | TokenEndwhile
           | TokenStmtEnd
           deriving (Show)

scanTokens = alexScanTokens

}