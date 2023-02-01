module Sequences (reSequenceTests) where

import AST
import qualified Data.Set as S
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

reSequenceTests :: TestTree
reSequenceTests =
  testGroup
    "--- Test Sequences ---"
    [ rudimentary ]

rudimentary :: TestTree
rudimentary =
  testGroup
    "Simple sequences"
    [ 
      testCase i1 $
        parseRE i1
          @?= Right re1,
      testCase i2 $
        parseRE i2
          @?= Right re2,
      testCase i3 $
        parseRE i3
          @?= Right re3,
      testCase i4 $
        parseRE i4
          @?= Right re4,
      testCase i5 $
        parseRE i5
          @?= Right re5,
      testCase i6 $
        parseRE i6
          @?= Right re6,
      testCase i7 $
        parseRE i7
          @?= Right re7
    ]
  where
    i1 = "a"
    i2 = "ab"
    i3 = "abc"
    i4 = "a*b"
    i5 = "(#a)b"
    i6 = "a|b"
    i7 = "a|bc"

    re1 = classFrom "a"
    re2 = RSeq [classFrom "a", classFrom "b"]
    re3 = RSeq [classFrom "a", classFrom "b", classFrom "c"]
    re4 = RSeq [RRepeat (classFrom "a") (0, maxBound::Int), classFrom "b"]
    re5 = RSeq [RCapture (classFrom "a"), classFrom "b"]
    re6 = RAlt (classFrom "a") (classFrom "b")
    re7 = RAlt (classFrom "a") (RSeq [classFrom "b", classFrom "c"])
    
classFrom :: String -> RE
classFrom input = 
  RClass False (S.fromList input)


