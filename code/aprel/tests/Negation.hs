module Negation (reNegationTests) where

import AST
import qualified Data.Set as S
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

reNegationTests :: TestTree
reNegationTests =
  testGroup
    "Test Negation !"
    [ rudimentary,
      shouldFail
    ]

rudimentary :: TestTree
rudimentary =
  testGroup
    "Simple expressions with REAlt"
    [ testCase "a!" $
        parseRE "a!"
          @?= Right re1,
      testCase "a!b!c" $
        parseRE "a!b!c"
          @?= Right re2,
      testCase "a!b!c!" $
        parseRE "a!b!c!"
          @?= Right re3,
      testCase "a!bc!" $
        parseRE "a!bc!"
          @?= Right re4,
      testCase "a!!" $
        parseRE "a!!"
          @?= Right re5,
      testCase "a!!!" $
        parseRE "a!!!"
          @?= Right re6
    ]
  where
    re1 = RNeg (RClass False (S.fromList "a")) 
    re2 = RSeq [RNeg (RClass False (S.fromList "a")), RNeg (RClass False (S.fromList "b")), RClass False (S.fromList "c")]
    re3 = RSeq [RNeg (RClass False (S.fromList "a")), RNeg (RClass False (S.fromList "b")), RNeg (RClass False (S.fromList "c"))]
    re4 = RSeq [RNeg (RClass False (S.fromList "a")), RClass False (S.fromList "b"), RNeg (RClass False (S.fromList "c"))]
    re5 = RNeg re1
    re6 = RNeg re5

testCaseBad :: Show a => String -> Either String a -> TestTree
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Left e -> return ()
      Right a -> assertFailure $ "Unexpected success: " ++ show a

shouldFail :: TestTree
shouldFail = 
  testGroup
    "Expressions with '!' that should fail"
    [ testCaseBad "!" $
        parseRE "!",
      testCaseBad "!a" $
        parseRE "!a"
    ]

    
