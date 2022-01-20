{
module LabelLink (replaceLabels) where
import Data.Either (fromRight)
import qualified Data.Map as Map
}

%wrapper "monadUserState"

@label = [A-Za-z][0-9A-Za-z]*

tokens :-
  \".*\"            { onGlob }
  @label ":"        { onLabel }
  "<" @label ">"    { onLabelAccess }
  .|[\n]            { onGlob }

{

type AlexUserState = (String, Map.Map String Int)

alexEOF = return ()

alexInitUserState :: AlexUserState
alexInitUserState = ("", Map.empty)

getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s,alex_ust s)

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> Right (s { alex_ust = (f $ alex_ust s) },())

runAlexScan :: String -> Either String AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

alexRescan input map = case f (AlexState {
  alex_bytes = [],
  alex_pos = alexStartPos,
  alex_inp = input,
  alex_chr = '\n',
  alex_scd = 0,
  alex_ust = ("", map)
}) of 
  Left msg -> error "Linking Failed"
  Right ( _, (newCode, _) ) -> newCode
  where
    Alex f = alexMonadScan >> getUserState


onLabel :: AlexAction()
onLabel ((AlexPn _ line _), _, _, s) len = modifyUserState (\(code, labelMap) -> (code, Map.insert labelName addr labelMap)) >> alexMonadScan
  where
    labelName = take (len-1) s
    addr = 2056 + 2 *(line-1)

onLabelAccess :: AlexAction()
onLabelAccess (_, _, _, s) len = modifyUserState (replaceLabel old) >> alexMonadScan   
  where 
    old = take len s
    replaceLabel :: String -> AlexUserState -> AlexUserState
    replaceLabel label (code, labelMap) = (code ++ new, labelMap)
      where
        key = init $ tail label
        new = case (Map.lookup key labelMap) of
          Nothing -> label
          Just addr -> show addr
      
onGlob :: AlexAction()
onGlob (_, _, _, s) len = modifyUserState (\(code, labelMap) -> (code ++ new, labelMap)) >> alexMonadScan   
  where
    new = take len s

replaceLabels::String->String
replaceLabels code = replacedCode
  where
    (codeNew , map) = fromRight (error "Linking failed") (runAlexScan code)
    replacedCode = alexRescan codeNew map

}