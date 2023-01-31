module BackReferences (backRefTests) where

import AST
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

backRefTests :: TestTree
backRefTests =
  testGroup
    "Test backreferences"
    [ alone
    ]

alone :: TestTree
alone =
  testGroup
    "Simple expressions with backreference"
    [ testCase "\\1" $
        parseRE "\\1"
          @?= Right (RBackref 1),
      testCase "\\11" $
        parseRE "\\11"
          @?= Right (RBackref 11)
    ]
