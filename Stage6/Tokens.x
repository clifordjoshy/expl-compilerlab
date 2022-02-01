{
module Tokens where
}

%wrapper "basic"

@integer = [0-9]+
@varName = [a-zA-Z_][a-zA-Z_0-9]*
@string = \"(\\.|[^\"\\])*\"

tokens :-

  $white+         ;
  @integer        { \s -> TokenIntVal (read s) }
  @string         { \s -> TokenStrVal s }
  \+              { \_ -> TokenPlus }
  \-              { \_ -> TokenMinus }
  \*              { \_ -> TokenTimes }
  \/              { \_ -> TokenDiv }
  \%              { \_ -> TokenMod }
  \(              { \_ -> TokenLParen }
  \)              { \_ -> TokenRParen }
  \=              { \_ -> TokenAssign }
  \<              { \_ -> TokenLt }
  \>              { \_ -> TokenGt }
  \<\=            { \_ -> TokenLtE }
  \>\=            { \_ -> TokenGtE }
  \!\=            { \_ -> TokenNE }
  \=\=            { \_ -> TokenEq }
  \.              { \_ -> TokenDot }
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
  decl            { \_ -> TokenDecl }
  enddecl         { \_ -> TokenEndDecl }
  int             { \_ -> TokenInt }
  str             { \_ -> TokenStr }
  main            { \_ -> TokenMain }
  return          { \_ -> TokenReturn }
  type            { \_ -> TokenType }
  endtype         { \_ -> TokenEndType }
  NULL            { \_ -> TokenNull }
  @varName        { \s -> TokenVar s }
  \[              { \_ -> TokenLBox }
  \]              { \_ -> TokenRBox }
  \{              { \_ -> TokenLCurly }
  \}              { \_ -> TokenRCurly }
  \&              { \_ -> TokenAddr }
  \,              { \_ -> TokenComma }
  \;              { \_ -> TokenStmtEnd }

{

data Token = TokenIntVal Int
           | TokenStrVal String
           | TokenVar String
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenMod
           | TokenLt
           | TokenLtE
           | TokenGt
           | TokenGtE
           | TokenEq
           | TokenNE
           | TokenLParen
           | TokenRParen
           | TokenAssign
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
           | TokenDecl
           | TokenEndDecl
           | TokenInt
           | TokenStr
           | TokenLBox
           | TokenRBox
           | TokenLCurly
           | TokenRCurly
           | TokenAddr
           | TokenMain
           | TokenReturn
           | TokenComma
           | TokenDot
           | TokenType
           | TokenEndType
           | TokenNull
           deriving (Show)

scanTokens = alexScanTokens

}