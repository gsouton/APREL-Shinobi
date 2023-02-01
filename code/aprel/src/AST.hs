module AST where

import qualified Data.Set as S

type CharSet = S.Set Char

data RE =
    RClass Bool{-complemented?-} CharSet
  | RSeq [RE]
  | RAlt RE RE
  | RRepeat RE (Int,Int)
  | RCapture RE
  | RBackref Int
  | RConj RE RE
  | RNeg RE
  deriving (Show, Eq)
  
-- matchRE :: RE -> Matcher Captures ()
-- matchRE (RClass isComplement charset) = do
--   c <- nextChar
--   if (isComplement && c `S.member` charset) || (not isComplement && c `S.notMember` charset)
--     then mfail
--     else return ()
--
-- matchRE (RSeq res) = foldl both (return ()) (map matchRE res)
-- matchRE (RAlt re1 re2) = pick [matchRE re1, matchRE re2]
-- matchRE (RRepeat re (minRepeat, maxRepeat)) = do
--   let match = matchRE re
--   if minRepeat == 0
--     then pick (replicate maxRepeat match ++ [return ()])
--     else both (sequence (replicate minRepeat match)) (matchRRepeat re (0, maxRepeat - minRepeat))
--
-- matchRE (RCapture re) = do
--   (captured, _, captures) <- grab (matchRE re)
--   putData (captures ++ [captured])
--
-- matchRE (RBackref i) = do
--   captures <- getData
--   if i >= length captures
--     then mfail
--     else do
--       let capture = captures !! i
--       let n = length capture
--       both (neg (pick (map matchRE (replicate n (RClass False (S.singleton '*')))))) (return ())
--
-- matchRE (RConj re1 re2) = both (matchRE re1) (matchRE re2)
-- matchRE (RNeg re) = neg (matchRE re)
--
-- matchTop :: RE -> String -> Maybe Captures
-- matchTop re str = case runMatcher (matchRE re) str 0 [] of
--   [(void, _, captures)] -> Just captures
--   _ -> Nothing
--
