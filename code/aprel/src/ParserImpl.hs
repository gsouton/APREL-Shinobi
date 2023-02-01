-- Put your parser implementation in this file
module ParserImpl where

import AST
-- import ReadP or Parsec, as relevant

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP

type Parser a = ReadP a -- may use synomym for easier portability to Parsec

-- Do not change the type!
parseRE :: String -> Either String RE
parseRE input = case readP_to_S (do res <- pRE; eof; return res) input of
  [(ast, "")] -> Right ast
  _s -> trace ("[parseRE]: failed with: " ++ show _s) Left "Parsing failed"

-- RE   :=  RESeq
--      |   RE '|' RE
--      |   RE '&' RE
-- !! left recursion
-- ReWrite to :
-- RE   :=  RESeq RE_
pRE :: Parser RE
pRE =
  do
    input <- look
    if input == ""
      then return (RSeq [])
      else do
        seq <- pRESeq
        pRE_ seq

-- {- trace ("[pRE]: Returns: " ++ show res) -} return res

-- a | b | c
-- RE_  :=  '|' RE
--      |   '&' RE
--      |   empty
pRE_ :: RE -> Parser RE
pRE_ seq =
  do
    char '&'
    seq' <- pRESeq
    pRE_ (RConj seq seq')
    -- rest <- look
    -- trace ("[pRE_]: Just parsed '|', calling pRE_ on : " ++ rest) pRE_ (RAlt seq seq')
    <|> do
      char '|'
      seq' <- pRESeq
      pRE_ (RAlt seq seq')
    <|> do
      return seq

-- RESeq    :=  REElt
--          |   empty
--          |   RESeq REseq
pRESeq :: Parser RE
pRESeq =
  do
    seqs <- many1 pREElt
    case seqs of
      [re] -> return re
      _ -> return (RSeq seqs)

-- REElt    :=  RERep
--          |   REElt '!'
-- This is left recursive....
-- So let's correct it to
--
-- REElt    :=  REREp REElt'
-- REElt'   :=  '!' REElt' | empty
pREElt :: Parser RE
pREElt =
  do
    pRERep

-- trace "[pREElt]: Calling pRERep" pRERep

pREElt_ :: RE -> Parser RE
pREElt_ rep =
  do
    char '!'
    return (RNeg rep)
    <|> do
      return rep

-- RERep    :=  REAtom
--          |   REAtom '{' Count '}'
--          |   REAtom '?'
--          |   REAtom '*'
--          |   REAtom '+'
pRERep :: Parser RE
pRERep =
  do
    atom <- pREAtom
    pRERepCount atom

-- rest <- look
-- (trace ("[pREElt]: Calling pRERepCount with rest input string: " ++ rest ++ ", atom: " ++ show atom)) pRERepCount atom

pRERepCount :: RE -> Parser RE
pRERepCount atom =
  do
    char '{'
    count <- pCount
    char '}'
    return (RRepeat atom count)
    -- trace ("[pRERepCount]: Trying to parse {}") return (RRepeat atom count)
    <|> do
      char '?'
      return (RRepeat atom (0, 1))
    -- trace ("[pRERepCount]: Trying to parse ?") return (RRepeat atom (0, 1))
    <|> do
      char '*'
      return (RRepeat atom (0, maxBound :: Int))
    -- trace ("[pRERepCount]: Trying to parse *") return (RRepeat atom (0, maxBound :: Int))
    <|> do
      char '+'
      return (RRepeat atom (1, maxBound :: Int))
    -- trace ("[pRERepCount]: Trying to parse +") return (RRepeat atom (1, maxBound :: Int))
    <|> do
      return atom

-- REAtom   :=  RChar
--          |   Class
--          |   '\' Number
--          |   '(' RE ')'
--          |   '(' '#' RE ')'
pREAtom :: Parser RE
pREAtom =
  do
    pClass
    <|> do
      char '\\'
      number <- pNumber
      return (RBackref number)
    -- rest <- look
    -- trace ("[pREAtom]: parsed backreference, number: " ++ show number ++ " ,rest:" ++ rest) return (RBackref number)
    <|> do
      char '('
      regex <- pRE
      char ')'
      return regex
    <|> do
      char '('
      char '#'
      regex <- pRE
      char ')'
      return (RCapture regex)
    <|> do
      c <- pRChar
      return (RClass False (S.singleton c))

-- Count    :=  Number
--          |   Number ','
--          |   Number ',' Number
pCount :: Parser (Int, Int)
pCount =
  do
    n <- pNumber
    return (n, maxBound :: Int)
    <|> do
      n <- pNumber
      char ','
      return (n, maxBound :: Int)
    <|> do
      n <- pNumber
      char ','
      m <- pNumber
      return (n, m)

-- Class    :=  '[' ClassItemz ']'
--          |   '[' '^' ClassItemz ']'
--          |   '.'
pClass :: Parser RE
pClass =
  do
    char '['
    items <- pClassItemz
    char ']'
    return (RClass False (S.fromList items))
    <|> do
      char '['
      char '^'
      items <- pClassItemz
      char ']'
      return (RClass True (S.fromList items))
    <|> do
      char '.'
      return (RClass True (S.fromList ""))

-- ClassItemz   :=  empty
--              |  ClassItem ClassItemz
pClassItemz :: Parser [Char]
pClassItemz =
  do
    l <- many pClassItem
    return (concat l)

-- ClassItem   :=  CChar
--              |   CChar '-' CChar
pClassItem :: Parser [Char]
pClassItem =
  do
    c <- pCChar
    return [c]
    <|> do
      begin <- pCChar
      char '-'
      end <- pCChar
      return [begin .. end]

-- RChar    := [ any character except the following: "!#&()*+.?[\{|"]
--          |   EscChar
pRChar :: Parser Char
pRChar =
  do
    satisfy (\c -> c `notElem` "!#&()*+.?[{|" && c /= '\\')
    -- (trace ("[pRChar]: parsed: " ++ show c)) (return c)
    <|> do
      pEscChar

-- CChar    := [ any character except the following : "-\]^"]
--          |   EscChar
pCChar :: Parser Char
pCChar =
  do
    satisfy (\c -> c `notElem` "-]^" && c /= '\\')
    <|> do pEscChar

-- EscChar  :=  '\' [any character excpet the following '0....9A..Za...z"]
--          |   '\' 'n'
--          |   '\' 't'
pEscChar :: Parser Char
pEscChar =
  do
    char '\\'
    satisfy (\c -> c `notElem` ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'])
    <|> do
      char '\n'
    <|> do
      char '\t'

pNumber :: Parser Int
pNumber =
  do
    res <- munch1 isDigit
    pure (read res)

-- Number   :=  Digit
--          |   Number Digit
-- Left Recursion !!!
-- Numberz  :=  Digit
--          |   Number Digit
-- pNumber :: Parser Int
-- pNumber =
--   do
--     digits <- many1 pDigit
--     pure (read digits)

-- Digit    := [any character from "0..9"]
-- pDigit :: Parser Char
-- pDigit =
--   do
--     satisfy isDigit
