module ConjunctionAndAlternation (reConjunctionAndAlternationTests) where

import AST
import qualified Data.Set as S
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

reConjunctionAndAlternationTests :: TestTree
reConjunctionAndAlternationTests =
  testGroup
    "--- Test Conjunction + Alternation '& |' ---"
    [ rudimentary ]

rudimentary :: TestTree
rudimentary =
  testGroup
    "Check for precedence between & and |"
    [ 
      testCase i1 $
        parseRE i1
          @?= Right re1
    ]
  where
    i1 = "a|b|c&de"
    re1 = RAlt (RAlt (classFrom "a") (classFrom "b") ) (RConj (classFrom "c") (RSeq [classFrom "d", classFrom "e"]))


classFrom :: String -> RE
classFrom input = 
  RClass False (S.fromList input)
