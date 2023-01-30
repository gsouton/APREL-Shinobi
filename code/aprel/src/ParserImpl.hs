-- Put your parser implementation in this file
module ParserImpl where

import AST
import qualified Data.Set as S
-- import ReadP or Parsec, as relevant
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>), Alternative (empty))
import Data.Text.Internal.Read (perhaps)


type Parser a = ReadP a -- may use synomym for easier portability to Parsec

-- Do not change the type!
parseRE :: String -> Either String RE
parseRE input = case readP_to_S (do pRE) input of
  [] -> Left "Cannot parse"
  [(regex, _)] -> Right regex
  _ -> error "could not parse"

-- RE   :=  RESeq
--      |   RE '|' RE
--      |   RE '&' RE
pRE :: Parser RE
pRE =
  do
    seq <- pRESeq
    return seq
      <|> do
        left <- pRE
        char '|'
        right <- pRE
        return (RAlt left right)
      <|> do
        left <- pRE
        char '&'
        right <- pRE
        return (RConj left right)

-- RESeq    :=  REElt
--          |   empty
--          |   RESeq REseq
pRESeq :: Parser RE
pRESeq = 
    do
        pREElt
        
-- REElt    :=  RERep
--          |   REElt '!'
pREElt :: Parser RE
pREElt = 
    do 
        pRERep
    <|> do
        regex <- pREElt;
        char '!'
        return (RNeg regex)

-- RERep    :=  REAtom
--          |   REAtom '{' Count '}'
--          |   REAtom '?'
--          |   REAtom '*'
--          |   REAtom '+'
pRERep :: Parser RE
pRERep = 
    do 
        pREAtom
    <|> do
        atom <- pREAtom;
        char '{';
        count <- pCount;
        char '}';
        return (RRepeat atom count)
    <|> do
        atom <- pREAtom
        char '?'
        return (RRepeat atom (0,1))
    <|> do
        atom <- pREAtom
        char '*'
        return (RRepeat atom (0, maxBound::Int))
    <|> do
        atom <- pREAtom
        char '+'
        return (RRepeat atom (1, maxBound::Int))

-- REAtom   :=  RChar 
--          |   Class
--          |   '\' Number
--          |   '(' RE ')'
--          |   '(' '#' RE ')'
pREAtom :: Parser RE 
pREAtom = undefined

-- Count    :=  Number
--          |   Number ','
--          |   Number ',' Number
pCount :: Parser (Int, Int)
pCount = undefined

-- Class    :=  '[' CassItemz ']'
--          |   '[' '^' pClassItemz ']'
--          |   '.'
pClass :: Parser RE
pClass = undefined

-- ClassItemz   :=  empty
--              |  ClassItem ClassItemz 
pClassItemz :: Parser RE
pClassItemz = undefined

-- pClassItem   :=  CChar 
--              |   CChar '-' CChar
pClassItem :: Parser RE
pClassItem = undefined

-- RChar    := [ any character except the following: "!#&()*+.?[\{|"]
--          |   EscChar
pRChar :: Parser RE
pRChar = undefined

-- CChar    := [ any character except the following : "-\]^"]
--          |   EscChar
pCChar :: Parser RE
pCChar = undefined

-- EscChar  :=  '\' [any character excpet the following '0....9A..Za...z"]
--          |   '\' 'n'
--          |   '\' 't'
pEscChar :: Parser RE
pEscChar = undefined

-- Number   :=  Digit
--          |   Number Digit
pNumber :: Parser Int
pNumber = undefined

-- Digit    := [any character from "0..9"]
pDigit :: Parser Int
pDigit = undefined
