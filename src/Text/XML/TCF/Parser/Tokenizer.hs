module Text.XML.TCF.Parser.Tokenizer
  ( Token (..)
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
  , tokenId :: Maybe Int
  , start :: Maybe TextPosition
  , end :: Maybe TextPosition
  , srcStart :: Maybe XmlPosition
  , srcEnd :: Maybe XmlPosition
  } deriving Show

-- | @tokenize@ is the tokenizer function. It takes a configuration
-- (cf. 'Config') as first parameter and a list of 'TcfElement's as
-- second parameters. It returns a list of 'Token's.
tokenize :: [Config] -> [TcfElement] -> [Token]
tokenize cfg tcf = tokenize' 1 tcf
  where
    -- We call _break_ a separating character.
    isBreak :: Char -> Bool
    isBreak c
      | c `elem` (getHyphens cfg) = False  -- hyphens do not separate tokens
      | otherwise = isSpace c || isPunctuation c -- spaces and punctuation do
    hasBreak :: String -> Bool
    hasBreak s = isJust $ find isBreak s

    -- tokenize' does all the work.
    tokenize' :: Int -- ^ token id (number)
              -> [TcfElement] -- ^ tcf elements left over
              -> [Token] -- ^ tokens returned
    tokenize' _ [] = []

    -- Drop Structure: when at 1st, 2nd, 3rd, 4th position. -- This
    -- should go as far as needed for the patterns below, which must
    -- not be interleaved with TcfStructures.
    tokenize' i ((TcfStructure _ _ _ _ _):xs) = tokenize' i xs
    tokenize' i (x:(TcfStructure _ _ _ _ _):xs) = tokenize' i (x:xs)
    tokenize' i (x:x2:(TcfStructure _ _ _ _ _):xs) = tokenize' i (x:x2:xs)
    tokenize' i (x:x2:x3:(TcfStructure _ _ _ _ _):xs) = tokenize' i (x:x2:x3:xs)
    
    -- Continueing Token: A continuing token spans more than one
    -- TcfText without break.
    tokenize' i ((TcfText t1 tOffset1 sOffset1) : (TcfText (t2:t2s) tOffset2 sOffset2) : xs)
      | (not $ null t1) && -- First text is non-empty.
        (not $ hasBreak t1) && -- First text is a token or less.
        (not $ isBreak t2) -- Text 2 does not start with break.
      -- We do not create a token, but move letters from the second
      -- text to the first. This will be repeated for tokens that span
      -- several text nodes.
      = tokenize' i ((TcfText
                      (t1 ++ take letters (t2:t2s))
                      tOffset1
                      {-sOffset1 FIXME-} 666)
                     :
                     (TcfText
                      (drop letters (t2:t2s))
                      (shiftTextPosition tOffset2 letters)
                      (shiftTextPosition sOffset2 letters))
                     : xs)
      where
        letters = length $ takeWhile (not . isBreak) (t2:t2s)

    -- Hyphenation At Linebreak:
    
    -- Hyphenation Case 1: Lines are enclosed in line-elements,
    -- e.g. <TEI:l>, and these lines are pretty printed each in one
    -- line of the xml-file. This results in i) a TcfText with
    -- trailing hyphen, ii) a TcfText with a newline and maybe other
    -- white space, iii) a TcfLineBreak, iv) the next text or
    -- structure elements. Here we just drop the white space (ii), so
    -- that this case is turned into Case 2.
    tokenize' i ((TcfText t1 tOffset1 sOffset1):(TcfText t2 _ _):(TcfLineBreak):xs)
      | (not $ null t1) && -- first text must be non-empty
        (not $ hasBreak t1) && -- may there be trailing whitespace?
        (elem (last t1) $ getHyphens cfg) && -- trailing hyphen in t1
        (null $ dropWhile isSpace t2) -- drop white space node after linebreak
      = tokenize' i ((TcfText t1 tOffset1 sOffset1):(TcfLineBreak):xs)

    -- Hyphenation Case 2: Lines are separated by an empty line break
    -- tag, e.g. <TEI:lb/> or <xhtml:br/>. This results in i) a
    -- TcfText with trailing hyphen, ii) a TcfLineBreak, iii) a
    -- TcfText with maybe white space in the head, e.g. a newline
    -- character like DTA's texts. Since the token may excede the
    -- first TcfText after the linebreak (iii), we will not produce a
    -- token here, but only a) drop the trailing hyphen character from
    -- (i), b) drop leading space form (iii), c) move letters from
    -- (iii) to (i) and d) drop the TcfLineBreak (ii). So the result
    -- in the first place is a TcfText consisting of letters
    -- only. This can be a continuing token, even when (iii) was only
    -- a newline character (and other white space) and the next
    -- letters where enclosed in a structure element,
    -- like in "diffe-<lb/>\n<em>rance</em>".
    tokenize' i ((TcfText t1 tOffset1 sOffset1):(TcfLineBreak):(TcfText t2 tOffset2 sOffset2):xs)
      | (not $ null t1) && -- first text must be non-empty
        (not $ hasBreak t1) && -- first text is a word. May there be trailing whitespace?
        (elem (last t1) $ getHyphens cfg) -- trailing hyphen in first text
      = tokenize' i ((TcfText
                      ((init t1) ++ (take letters $ drop spaces t2))
                      tOffset1
                      sOffset1)
                     :
                     (TcfText
                      (drop (letters+spaces) t2)
                      (shiftTextPosition tOffset2 (letters+spaces))
                      (shiftXmlPosition sOffset2 (letters+spaces)))
                     : xs)
      where
        spaces = length $ takeWhile isSpace t2
        letters = length $ takeWhile (not . isBreak) (drop spaces t2)

    -- Drop linebreaks that were not preceded by a hyphen
    tokenize' i ((TcfLineBreak):xs) = tokenize' i xs

    -- Drop zero length text node.
    tokenize' i ((TcfText "" _ _):xs)
      = tokenize' i xs

    -- Make tokens by eating a single TcfText element. We prefer
    -- pattern matching with destructuring the text into (t:ts) over
    -- getting the head of the text in a guard.
    tokenize' i ((TcfText (t:ts) tOffset sOffset):xs)
      -- Drop heading space
      | isSpace t
      = tokenize' i ((TcfText
                      (drop spaces ts)
                      (shiftTextPosition tOffset spaces+1)
                      (shiftXmlPosition sOffset spaces+1))
                     : xs)
      -- Punctuation token
      | isPunctuation t
      = (Token
         (t:[])
         (Just i)
         (Just tOffset) Nothing
         (Just sOffset) Nothing)
        : (tokenize' (i+1)
           ((TcfText
             ts
             (shiftTextPosition tOffset 1)
             (shiftXmlPosition sOffset 1))
            : xs))
      -- Word token. Letters here means letters, digits, marks etc., but
      -- neither spaces nor punctuation.
      | otherwise
      = (Token
         (t:(take letters ts))
         (Just i)
         (Just tOffset) Nothing
         (Just sOffset) Nothing)
        : (tokenize' (i+1)
           ((TcfText
             (drop letters ts)
             (shiftTextPosition tOffset letters+1)
             (shiftXmlPosition sOffset letters+1))
            : xs))
      where
        spaces = length $ takeWhile isSpace ts
        letters = length $ takeWhile (not . isBreak) ts
