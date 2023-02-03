module MatcherTest (matcherTests) where

import AST
import qualified Data.Set as S
import Matcher
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))
import GHC.Enum (Bounded(maxBound))
import AST (RE(RCapture, RRepeat))

matcherTests :: TestTree
matcherTests =
  testGroup
    "--- Test on classes  ---"
    [ 
      rudimentary,
      alternation,
      conjunction,
      altAndConj,
      negation

    ]

rudimentary :: TestTree
rudimentary =
  testGroup
    "Matching rudimentary"
    [
      testCase i1 $
        matchTop re1 i1
          @?= Nothing,

      testCase i2 $
        matchTop re2 i2
          @?= Nothing,

      testCase i3 $
        matchTop re2 i3
          @?= Just [i3],

      testCase i4 $
        matchTop re2 i4
          @?= Just [i4],

      testCase i5 $
        matchTop re5 i5
          @?= Nothing,

      testCase i6 $
        matchTop re6 i6
          @?= Nothing,

      testCase i7 $
        matchTop re7 i7
          @?= Just [i7]
    ]
  where
    i1 = "a"
    re1 = RClass False (S.singleton 'a')

    i2 = "aaa"
    re2 = RSeq [re1, re1, re1]

    i3 = "a"
    re3 = RCapture re1

    i4 = "aaa"
    re4 = RSeq [re3, re3, re3]

    i5 = "b"
    re5 = RClass False (S.singleton 'b')

    i6 = "aba"
    re6 = RSeq [re1, re5, re1]

    i7 = "aaaaaabbbbbb"
    re7 = RSeq [RCapture (repeatInf "a") , RCapture (repeatInf "b")]


repeatInf :: String -> RE
repeatInf str = RRepeat  (RClass False (S.fromList str)) (0,maxBound::Int)

repeatNum :: RE -> Int -> RE
repeatNum re m = RRepeat re (0, m)

toClass :: String -> RE
toClass str = RClass False (S.fromList str)

alt :: String -> String -> RE
alt left right =
  RAlt (toClass left) (toClass right)


conj :: String -> String -> RE
conj left right =
  RConj (toClass left) (toClass right)

capture :: RE -> RE
capture re =
  RCapture re


alternation :: TestTree
alternation =
  testGroup
    "Matching top with alternation"
    [
      testCase i1 $
        matchTop re1 i1
          @?= Just [i1],

      testCase i2 $
        matchTop re2 i2
          @?= Just[i2],

      testCase i3 $
        matchTop re2 i3
          @?= Nothing
    ]
  where
    i1 = "a"
    re1 = capture (alt "a" "b")

    i2 = "aaa"
    re2 = repeatNum (capture (RAlt (alt "b" "c") (alt "d" "a"))) 3

    i3 = "a"
    re3 = alt "b" "a" 



conjunction :: TestTree
conjunction =
  testGroup
    "Matching top with conjunction"
    [
      testCase i1 $
        matchTop re1 i1
          @?= Just [i1],

      testCase i2 $
        matchTop re2 i2
          @?= Nothing,

      testCase i3 $
        matchTop re2 i3
          @?= Nothing
    ]
  where
    i1 = "a"
    re1 = capture (conj "a" "b")

    i2 = "aaa"
    re2 = repeatNum (capture (RConj (conj "b" "c") (conj "d" "a"))) 3

    i3 = "a"
    re3 = conj "b" "a" 


altAndConj :: TestTree
altAndConj =
  testGroup
    "Alternation and Conjunction"
    [
      testCase i1 $
        matchTop re1 i1
          @?= Nothing,

      testCase i2 $
        matchTop re2 i2
          @?= Nothing,

      testCase i3 $
        matchTop re2 i3
          @?= Just[i3]
    ]
  where
    i1 = "a"
    re1 = RConj (capture (alt "a" "b")) (alt "b" "c")

    i2 = "aaa"
    re2 = repeatNum (capture (RConj (alt "b" "c") (alt "d" "a"))) 3

    i3 = "ab"
    re3 = repeatNum (RCapture (RConj (alt "b" "a") (alt "b" "a"))) 2


negation :: TestTree
negation =
  testGroup
    "Negation"
    [
      testCase i1 $
        matchTop re1 i1
          @?= Just[i1],

      testCase i2 $
        matchTop re1 i2
          @?= Just[i2],

      testCase i3 $
        matchTop re1 i3
          @?= Just[i3]
    ]
  where
    i1 = ""
    re1 = RNeg (RSeq [RCapture (RClass False (S.fromList "a")),RCapture (RRepeat (RClass False (S.fromList "b")) (0,9223372036854775807))])

    i2 = "ba"
    i3 = "c"

