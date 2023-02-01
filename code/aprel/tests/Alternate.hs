module Alternate (reAltTests) where

import AST
import qualified Data.Set as S
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

reAltTests :: TestTree
reAltTests =
  testGroup
    "--- Test Alternation '|' ---"
    [ rudimentary,
      shouldFail
    ]

rudimentary :: TestTree
rudimentary =
  testGroup
    "Simple expressions with REAlt"
    [ testCase "a|b" $
        parseRE "a|b"
          @?= Right re1,
      testCase "a|b|c" $
        parseRE "a|b|c"
          @?= Right re2
    ]
  where
    re1 = RAlt (RClass False (S.fromList "a")) (RClass False (S.fromList "b"))
    re2 = RAlt (RAlt (RClass False (S.fromList "a")) (RClass False (S.fromList "b"))) (RClass False (S.fromList "c"))

testCaseBad :: Show a => String -> Either String a -> TestTree
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Left e -> return ()
      Right a -> assertFailure $ "Unexpected success: " ++ show a

shouldFail :: TestTree
shouldFail = 
  testGroup
    "Expressions with '|' that should fail"
    [ testCaseBad "|" $
        parseRE "|",
      testCaseBad "|a" $
        parseRE "|a",
      testCaseBad "a|" $
        parseRE "a|",
      testCaseBad "a||" $
        parseRE "a||",
      testCaseBad "|a|b" $
        parseRE "|a|b",
      testCaseBad "a|b|" $
        parseRE "a|b|",
      testCaseBad "a|b||c" $
        parseRE "a|b||c"
    ]

    
