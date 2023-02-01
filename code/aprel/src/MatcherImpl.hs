-- Put your matcher implementation in this file
module MatcherImpl where

import AST
import Control.Monad
import qualified Data.Set as S
import Debug.Trace (trace)

-- Generic string-matching monad. Do not change anything in
-- the following definitions.

newtype Matcher d a = Matcher {runMatcher :: String -> Int -> d -> [(a, Int, d)]}

instance Monad (Matcher d) where
  return a = Matcher (\_s _i d -> return (a, 0, d))
  m >>= f =
    Matcher
      ( \s i d -> do
          (a, j, d') <- runMatcher m s i d
          (b, j', d'') <- runMatcher (f a) s (i + j) d'
          return (b, j + j', d'')
      )

instance Functor (Matcher d) where fmap = liftM

instance Applicative (Matcher d) where pure = return; (<*>) = ap

-- Associated operations to implement. Their definitions may freely use
-- the Matcher term constructor. Do not change the types!

nextChar :: Matcher d Char
nextChar = Matcher (\s i d -> if i >= length s then [] else [(s !! i, i + 1, d)])

getData :: Matcher d d
getData = Matcher (\_s i d -> return (d, i, d))

putData :: d -> Matcher d ()
putData d = Matcher (\_s i _ -> return ((), i, d))

mfail :: Matcher d a
mfail = Matcher (\_s _i _d -> [])

pick :: [Matcher d a] -> Matcher d a
pick ms = Matcher (\s i d -> do concat [runMatcher m s i d | m <- ms])

grab :: Matcher d a -> Matcher d (String, a)
grab m = Matcher (\s i d -> do (a, j, d') <- runMatcher m s i d; return ((take j (drop i s), a), j, d'))

both :: Matcher d a -> (a -> Matcher d b) -> Matcher d b
both m1 m2 = Matcher (\s i d -> do (a, j, d') <- runMatcher m1 s i d; runMatcher (m2 a) s (i + j) d')

neg :: Matcher d a -> Matcher d ()
neg m = Matcher (\s i d -> let n = length s - i in if null (runMatcher m s i d) then return ((), n, d) else [])

-- APREL-specific functions to implement. `matchRE` should NOT use the Matcher
-- term constructor or the `runMatcher` projection, only the above functions.

type Captures = [String]

matchRE :: RE -> Matcher Captures ()
matchRE (RClass isComplement charSet) =
  do
    c <- nextChar
    when
      ( (isComplement && c `S.member` charSet)
          || (not isComplement && c `S.notMember` charSet)
      )
      mfail
matchRE (RSeq []) = return ()
matchRE (RSeq (x : xs)) = do
  matchRE x
  matchRE (RSeq xs)
matchRE (RAlt re1 re2) = pick [matchRE re1, matchRE re2]
matchRE (RRepeat re (minRep, maxRep)) =
  when (maxRep > minRep) $
    do
      matchRE re
      matchRE (RRepeat re (minRep, maxRep - 1))
matchRE (RCapture re) = do
  (captured, _) <- grab (matchRE re)
  putData [captured]
matchRE (RBackref i) = do
  captures <- getData
  if i >= length captures
    then mfail
    else do
      let capture = captures !! i
      matchRE (RClass False (S.fromList capture))
matchRE (RConj _re1 _re2) = undefined
matchRE (RNeg re) = neg (matchRE re)

matchTop :: RE -> String -> Maybe Captures
matchTop re str = case runMatcher (matchRE re) str 0 [] of
  [(_a, _i, captures)] -> trace ("successMatchTop" ++ show captures) Just captures
  s -> trace ("failedMatchTop: " ++ show s) Nothing
