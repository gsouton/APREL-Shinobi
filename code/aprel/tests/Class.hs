module Class (reClassTests) where

import AST
import qualified Data.Set as S
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Token (GenTokenParser (whiteSpace))

reClassTests :: TestTree
reClassTests =
  testGroup
    "--- Test on classes  ---"
    [ rudimentary,
      ranges
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
          @?= Right re4
    ]
  where
    i1 = "a"
    i2 = "abc"
    re1 = RClass False (S.singleton 'a')
    re2 = RClass False (S.singleton 'b')
    re3 = RClass False (S.singleton 'c')
    re4 = RSeq [re1, re2, re3]




testCaseBad :: Show a => String -> Either String a -> TestTree
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Left e -> return ()
      Right a -> assertFailure $ "Unexpected success: " ++ show a

ranges :: TestTree
ranges = 
  testGroup
    "test on ranges"
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
          @?= Right re5
    ]
    where
    i1 = "[a-z]"
    i2 = "[A-Za-z0-9]"
    i3 = "[c-a]"
    i4 = "[b-b]"
    i5 = "[^]"

    re1 = RClass False (S.fromList ['a'..'z'])
    re2 = RClass False (S.fromList (['a'..'z']++['A'..'Z']++['0'..'9']))
    re3 = RClass False (S.fromList [])
    re4 = RClass False (S.fromList ['b'..'b'])
    re5 = RClass True (S.fromList "")
