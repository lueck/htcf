module Text.XML.TCF.Parser.Tokenizer
  ( Token
  , tokenize
  ) where

import Data.Char
import Data.List
import Data.Maybe

import Text.XML.TCF.Parser.Position
import Text.XML.TCF.Parser.TcfElement

data Token =
  Token
  { token :: String
  , textOffset :: TextPosition
  , srcOffset :: XmlPosition
  } deriving Show

-- FIXME: Not needed? inexhaustive pattern
eatChars :: Int -> TcfElement -> (Token, TcfElement)
eatChars c (TcfText text tOffset sOffset) =
  ( (Token (take i text) tOffset sOffset)
  , (TcfText (drop i text) (shiftTextPosition tOffset i) (shiftXmlPosition sOffset i)))
  where
    i = max c $ length text

isEmptyToken :: Token -> Bool
isEmptyToken (Token t _ _) = t == "" 

hasSpace :: String -> Bool
hasSpace s = isJust $ find isSpace s

hasPunctuation :: String -> Bool
hasPunctuation s = isJust $ find isPunctuation s

-- | We call _break_ a separating character.
--
-- @isBreak@ is a predicate that returns @True@ for a breaking
-- character.
isBreak :: Char -> Bool
isBreak c = isSpace c || isPunctuation c

hasBreak :: String -> Bool
hasBreak s = hasSpace s || hasPunctuation s

tokenize :: [TcfElement] -> [Token]
tokenize [] = []
-- drop structure
tokenize ((TcfStructure _ _ _ _ _):xs) = tokenize xs
-- drop structure
tokenize ((TcfText t tOffset sOffset):(TcfStructure _ _ _ _ _):xs)
  = tokenize ((TcfText t tOffset sOffset) : xs)
tokenize ((TcfText t1 tOffset1 sOffset1) : (TcfText t2 tOffset2 sOffset2) : xs)
  -- continueing token, i.e. a token that spans more than one TcfText
  | (0 < length t1) && (not $ hasBreak t1) && (not $ isBreak $ head t2)
  = (Token (t1 ++ take letters t2) tOffset1 {-sOffset1 FIXME-} 666)
    : tokenize ((TcfText
                 (drop letters t2)
                 (shiftTextPosition tOffset2 letters)
                 (shiftTextPosition sOffset2 letters))
                : xs)
  -- token not continued in next TcfText
  | (0 < length t1) && (not $ hasBreak t1) && (isBreak $ head t2)
  = (Token t1 tOffset1 {-sOffset1 FIXME-} 777)
    : tokenize ((TcfText t2 tOffset2 sOffset2) : xs)
  where
    letters = length $ takeWhile (not . isBreak) t2
tokenize ((TcfText t tOffset sOffset):xs)
  -- drop zero length string
  | t == "" = tokenize xs
  -- drop heading space
  | isSpace $ head t = tokenize ((TcfText
                                  (drop spaces t)
                                  (shiftTextPosition tOffset spaces)
                                  (shiftXmlPosition sOffset spaces))
                                 : xs)
  -- punctuation token
  | isPunctuation $ head t = (Token ((head t):[]) tOffset sOffset)
                             : (tokenize
                                ((TcfText
                                  (tail t)
                                  (shiftTextPosition tOffset 1)
                                  (shiftXmlPosition sOffset 1))
                                 : xs))
  -- word token
  | otherwise = (Token (take letters t) tOffset sOffset)
                : (tokenize
                   ((TcfText
                     (drop letters t)
                     (shiftTextPosition tOffset letters)
                     (shiftXmlPosition sOffset letters))
                    : xs))
  where
    spaces = length $ takeWhile isSpace t
    letters = length $ takeWhile (not . isBreak) t
