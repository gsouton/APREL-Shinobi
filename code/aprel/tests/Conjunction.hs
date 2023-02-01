module Conjunction (reConjunctionTests) where

import AST
import qualified Data.Set as S
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

reConjunctionTests :: TestTree
reConjunctionTests =
  testGroup
    "--- Test Conjunction '&' ---"
    [ rudimentary,
      shouldFail
    ]

rudimentary :: TestTree
rudimentary =
  testGroup
    "Simple expressions with Conjunction '&'"
    [ 
      testCase i1 $
        parseRE i1
          @?= Right re1,
      testCase i2 $
        parseRE i2
          @?= Right re2
    ]
  where
    i1 = "a&b"
    i2 = "a&b&c"
    re1 = RConj (RClass False (S.fromList "a")) (RClass False (S.fromList "b"))
    re2 = RConj (RConj (RClass False (S.fromList "a")) (RClass False (S.fromList "b"))) (RClass False (S.fromList "c"))




testCaseBad :: Show a => String -> Either String a -> TestTree
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Left e -> return ()
      Right a -> assertFailure $ "Unexpected success: " ++ show a

shouldFail :: TestTree
shouldFail = 
  testGroup
    "Expressions with Conjunction & that should fail"
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
        parseRE i8
    ]
    where
    i1 = "&"
    i2 = "&a"
    i3 = "a&"
    i4 = "a&&"
    i5 = "&a&b"
    i6 = "a&b&"
    i7 = "a&b&c&&"
    i8 = "a&b&&c"
