module Text.XML.TCF.Parser.Tokenizer
  ( Token
  , tokenize
  ) where

import Data.Char
import Data.List
import Data.Maybe

import Text.XML.TCF.Parser.Position
import Text.XML.TCF.Parser.TcfElement
import Text.XML.TCF.Parser.ConfigParser

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
hasBreak s = isJust $ find isBreak s

-- | @tokenize@ is the tokenizer function. It takes a configuration
-- (cf. 'Config') and a list of 'TcfElement's as parameters and
-- returns a list of 'Token's.
tokenize :: [Config] -> [TcfElement] -> [Token]
tokenize cfg tcf = tokenize' tcf
  where
    tokenize' :: [TcfElement] -> [Token]
    tokenize' [] = []
    -- drop structure: when at head, second, third, fourth. This
    -- should go as far as needed for the patterns below, which must
    -- not be interleaved with TcfStructures.
    tokenize' ((TcfStructure _ _ _ _ _):xs) = tokenize' xs
    tokenize' (x:(TcfStructure _ _ _ _ _):xs) = tokenize' (x:xs)
    tokenize' (x:x2:(TcfStructure _ _ _ _ _):xs) = tokenize' (x:x2:xs)
    tokenize' (x:x2:x3:(TcfStructure _ _ _ _ _):xs) = tokenize' (x:x2:x3:xs)
    -- continueing token
    tokenize' ((TcfText t1 tOffset1 sOffset1) : (TcfText t2 tOffset2 sOffset2) : xs)
      -- a continueing token spans more than one TcfText without break
      | (not $ null t1) && (not $ hasBreak t1) && (not $ isBreak $ head t2)
      = (Token (t1 ++ take letters t2) tOffset1 {-sOffset1 FIXME-} 666)
        : tokenize' ((TcfText
                      (drop letters t2)
                      (shiftTextPosition tOffset2 letters)
                      (shiftTextPosition sOffset2 letters))
                     : xs)
      -- token not continued in next TcfText; FIXME: not needed!?
      | (not $ null t1) && (not $ hasBreak t1) && (isBreak $ head t2)
      = (Token t1 tOffset1 {-sOffset1 FIXME-} 777)
        : tokenize' ((TcfText t2 tOffset2 sOffset2) : xs)
      where
        letters = length $ takeWhile (not . isBreak) t2
    -- hyphenation at linebreak
    tokenize' ((TcfText t1 tOffset1 sOffset1):(TcfLineBreak):(TcfText t2 tOffset2 sOffset2):xs)
      -- case: drop white space text node after linebreak
      | (not $ null t1) && -- first text must be non-empty
        (not $ hasBreak t1) && -- may there be trailing whitespace?
        (elem (last t1) $ getHyphens cfg) && -- trailing hyphen in t1
        (null $ dropWhile isSpace t2) -- drop white space node after linebreak
      = tokenize' ((TcfText t1 tOffset1 sOffset1):(TcfLineBreak):xs)
      -- case: continue token after linebreak
      | (not $ null t1) && -- first text must be non-empty
        (not $ hasBreak t1) && -- first text is a word. May there be trailing whitespace?
        (elem (last t1) $ getHyphens cfg) -- trailing hyphen in first text
      = (Token
         ((init t1) ++ (take letters $ drop spaces t2))
         tOffset1 sOffset1)
        : (tokenize' ((TcfText
                       (drop (letters+spaces) t2)
                       (shiftTextPosition tOffset2 (letters+spaces))
                       (shiftXmlPosition sOffset2 (letters+spaces)))
                      : xs))
      where
        spaces = length $ takeWhile isSpace t2
        letters = length $ takeWhile (not . isBreak) (drop spaces t2)
    -- drop linebreaks that were not preceded by a hyphen
    tokenize' ((TcfLineBreak):xs) = tokenize' xs
    -- cases not spanning multiple TcfTexts
    tokenize' ((TcfText t tOffset sOffset):xs)
      -- drop zero length string
      | t == "" = tokenize' xs
      -- drop heading space
      | isSpace $ head t = tokenize' ((TcfText
                                       (drop spaces t)
                                       (shiftTextPosition tOffset spaces)
                                       (shiftXmlPosition sOffset spaces))
                                      : xs)
      -- punctuation token
      | isPunctuation $ head t = (Token ((head t):[]) tOffset sOffset)
                                 : (tokenize'
                                    ((TcfText
                                      (tail t)
                                      (shiftTextPosition tOffset 1)
                                      (shiftXmlPosition sOffset 1))
                                     : xs))
      -- word token. Letters here means letters, digits, marks etc., but
      -- neither spaces nor punctuation
      | otherwise = (Token (take letters t) tOffset sOffset)
                    : (tokenize'
                       ((TcfText
                         (drop letters t)
                         (shiftTextPosition tOffset letters)
                         (shiftXmlPosition sOffset letters))
                        : xs))
      where
        spaces = length $ takeWhile isSpace t
        letters = length $ takeWhile (not . isBreak) t
