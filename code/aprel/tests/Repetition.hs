module Repetition (reRepetitionTests) where

import AST
import qualified Data.Set as S
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

reRepetitionTests :: TestTree
reRepetitionTests =
  testGroup
    "--- Test Repetitions (*, +, ?, {n, m}) ---"
    [ rudimentary,
      shouldFail
    ]

rudimentary :: TestTree
rudimentary =
  testGroup
    "Simple expressions with RERep"
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
          @?= Right re7,
      testCase i8 $
        parseRE i8
          @?= Right re8,
      testCase i9 $
        parseRE i9
          @?= Right re9,
      testCase i10 $
        parseRE i10
          @?= Right re10,
      testCase i11 $
        parseRE i11
          @?= Right re11
    ]
  where
    i1 = "a*"
    i2 = "a?"
    i3 = "a+"
    i4 = "a{0,}"
    i5 = "a{0,1}"
    i6 = "a{1,}"
    i7 = "a{1,12}"
    i8 = "a{22,42}"
    i9 = "a{5,0}"
    i10 = "a{1}"
    i11 = "a{0}"

    re1 = RRepeat (RClass False (S.fromList "a")) (0, maxBound::Int)
    re2 = RRepeat (RClass False (S.fromList "a")) (0, 1)
    re3 = RRepeat (RClass False (S.fromList "a")) (1, maxBound::Int)
    re4 = re1
    re5 = re2
    re6 = re3
    re7 = RRepeat (RClass False (S.fromList "a")) (1, 12)
    re8 = RRepeat (RClass False (S.fromList "a")) (22, 42)
    re9 = RRepeat (RClass False (S.fromList "a")) (5, 0)
    re10 = re3
    re11 = re1




testCaseBad :: Show a => String -> Either String a -> TestTree
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Left e -> return ()
      Right a -> assertFailure $ "Unexpected success: " ++ show a

shouldFail :: TestTree
shouldFail = 
  testGroup
    "Expressions with repetition (*, +, ? {n, m}..) that should fail"
    [ 
      testCaseBad i1 $
        parseRE i1,
      testCaseBad i2 $
        parseRE i2,
      testCaseBad i3 $
        parseRE i3,
      testCaseBad i4 $
        parseRE i4,
      testCaseBad i5 $
        parseRE i5,
      testCaseBad i6 $
        parseRE i6,
      testCaseBad i7 $
        parseRE i7,
      testCaseBad i8 $
        parseRE i8,
      testCaseBad i9 $
        parseRE i9,
      testCaseBad i10 $
        parseRE i10,
      testCaseBad i11 $
        parseRE i11,
      testCaseBad i12 $
        parseRE i12,
      testCaseBad i13 $
        parseRE i13,
      testCaseBad i14 $
        parseRE i14,
      testCaseBad i15 $
        parseRE i15,
      testCaseBad i16 $
        parseRE i16,
      testCaseBad i17 $
        parseRE i17,
      testCaseBad i18 $
        parseRE i18,
      testCaseBad i19 $
        parseRE i19,
      testCaseBad i20 $
        parseRE i20,
      testCaseBad i21 $
        parseRE i21,
      testCaseBad i22 $
        parseRE i22,
      testCaseBad i23 $
        parseRE i23,
      testCaseBad i24 $
        parseRE i24,
      testCaseBad i25 $
        parseRE i25,
      testCaseBad i26 $
        parseRE i26,
      testCaseBad i27 $
        parseRE i27,
      testCaseBad i28 $
        parseRE i28,
      testCaseBad i29 $
        parseRE i29,
      testCaseBad i30 $
        parseRE i30
    ]
    where
    i1 = "{"
    i2 = "a{"
    i3 = "a{}"
    i4 = "a{a,b}"
    i5 = "a{a|b}"
    i6 = "a{{1,2}"
    i7 = "a{1,2{}"
    i8 = "a{1,2}{"
    i9 = "a{1,2}{1}"
    i10 = "a{*}"
    i11 = "a{?}"
    i12 = "a{+}"
    i13 = "a{-}"
    i14 = "a{(#b)}"
    i15 = "a{(1,2)}"
    i16 = "a{1,2)}"
    i17 = "a{(1,2}"
    i18 = "a{1,2,1}"
    i19 = "a{,2,1}"
    i20 = "a{2,1,}"
    i21 = "a{1,*2}"
    i22 = "a{1,&2}"
    i23 = "a{1,a2}"
    i24 = "a{1, 2}"
    i25 = "a**"
    i26 = "a+*"
    i27 = "a*+"
    i28 = "a*?"
    i29 = "a?*"
    i30 = "a?*"

    
