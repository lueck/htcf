{-# LANGUAGE Rank2Types #-}
module HTCF.Tokenizer
  ( Token (..)
  , tokenize
  , nthWordStart
  , dropNWords
  , isNumDay
  , isNumMonth
  , isRealAbbrev
  ) where

import Data.Char
import Data.List
import Data.Maybe

import HTCF.TokenLayer
import HTCF.Position
import HTCF.TcfParserTypeDefs
import HTCF.Config

isNumDay :: String -> Bool
isNumDay [] = False
isNumDay day
  | last day == '.' = isDay (init day)
  | otherwise = False
  where
    isDay (d:[]) = isDigit d
    isDay ('0':d:[]) = isDigit d
    isDay ('1':d:[]) = isDigit d
    isDay ('2':d:[]) = isDigit d
    isDay ('3':d:[]) = d == '0' || d == '1'
    isDay _ = False
{-# INLINE isNumDay #-}

isNumMonth :: String -> Bool
isNumMonth [] = False
isNumMonth month
  | last month == '.' = isMonth (init month)
  | otherwise = False
  where
    isMonth (d:[]) = isDigit d
    isMonth ('0':d:[]) = isDigit d
    isMonth ('1':d:[]) = d == '0' || d == '1' || d == '2'
    isMonth _ = False
{-# INLINE isNumMonth #-}

-- | Returns true, if the test string is a real subsequence of an
-- abbreviation string, i.e. it is shorter than the matching
-- abbreviation string.
isRealAbbrev :: [String] -> String -> Bool
isRealAbbrev [] _ = False
isRealAbbrev _ [] = False
isRealAbbrev abbrs str
  | (last str == '.') && (str /= ".") = substringP abbrs strWithoutDot
  | otherwise = False
  where
    strWithoutDot = init str
    lengthS = length strWithoutDot
    substringP :: [String] -> String -> Bool
    substringP [] _ = False
    substringP (a:as) s
      | (s `isPrefixOf` a) && (lengthS < length a) = True
      | otherwise = substringP as s

dropNWords :: (Char -> Bool) -> Int -> String -> String
dropNWords _ 0 s = s
dropNWords p n s = dropNWords p (n-1) $ dropWhile (not . p) $ dropWhile p s

nthWordStart :: (Char -> Bool) -> Int -> String -> Int
nthWordStart spacesP 0 s = length $ takeWhile spacesP s
nthWordStart spacesP n s = spacesPlusLetters + (nthWordStart spacesP (n-1) $ drop spacesPlusLetters s)
  where
    spaces = length $ takeWhile spacesP s
    letters = length $ takeWhile (not . spacesP) $ drop spaces s
    spacesPlusLetters = spaces+letters
{-# INLINE nthWordStart #-}

-- | @tokenize@ is the tokenizer function. It takes a configuration
-- (cf. 'Config') as first parameter and a list of 'TcfElement's as
-- second parameters. It returns a list of 'Token's.
tokenize :: Config -> [TcfElement] -> [Token]
tokenize cfg tcf = tokenize' 1 tcf
  where
    -- get configuration aspects
    nonBreakingChars = _cfg_noBreak cfg
    mkSingleDigitOrdinalP = _cfg_singleDigitOrdinal cfg
    hyphens = _cfg_hyphens cfg
    mk1CharAbbrevP = _cfg_abbrev1CharToken cfg
    months = _cfg_months cfg
    abbrevs = map (++ ".") $ _cfg_abbreviations cfg

    -- We call _break_ a separating character.
    isBreak :: Char -> Bool
    isBreak c
      | c `elem` hyphens = False  -- hyphens do not separate tokens
      | c `elem` nonBreakingChars = False -- non-breaking characters from config
      | otherwise = isSpace c || isPunctuation c -- spaces and punctuation do
    hasBreak :: String -> Bool
    --hasBreak = isJust . find isBreak
    hasBreak = any isBreak

    isAbbrev :: String -> Bool
    -- the abbreviations from the config have not dot.
    isAbbrev str = str `elem` abbrevs

    isLitMonthAbbrev :: String -> Bool
    isLitMonthAbbrev = isRealAbbrev months

    mkToken :: String -- the token
            -> Int -- the token ID
            -> TcfElement -- the tcf element with position
            -> Int -- offset in current string (text node)
            -> Token
    mkToken tok idd el 0 =
      (Token tok (Just idd)
         (Just tOffset)
         (Just $ shiftTextPosition tOffset $ length tok - 1)
         (fst $ head xOffset)
         (snd $ last $ take (length tok) xOffset))
      where
        tOffset = getTextOffset el
        xOffset = getSrcCharOffsets el
    mkToken tok idd el offset =
      (Token tok (Just idd)
         (Just $ shiftTextPosition tOffset offset)
         (Just $ shiftTextPosition tOffset $ offset + length tok)
         (fst $ xOffset !! offset)
         (snd $ last $ take (offset + length tok) xOffset))
      where
        tOffset = getTextOffset el
        xOffset = getSrcCharOffsets el

    mkText :: TcfElement     -- the old text
           -> Int            -- number of chars to drop from the old text (text node)
           -> TcfElement
    mkText (TcfText txt tOffset xOffset) n =
      (TcfText
       (drop n txt)
       (shiftTextPosition tOffset n)
       (drop n xOffset))  
    {-# INLINE mkText #-}

    mvTextLeft :: TcfElement   -- the old first (left) text
               -> TcfElement   -- the old second (right) text
               -> Int          -- numbers of chars to drop from the
                               -- right
               -> [TcfElement] -- returns two new text elements
    mvTextLeft (TcfText t1 tO1 xO1) (TcfText t2 tO2 xO2) i  =
      [ (TcfText
         (t1++(take i t2))
         tO1
         (xO1++(take i xO2)))
      , (TcfText
         (drop i t2)
         (shiftTextPosition tO2 i)
         (drop i xO2))]
    {-# INLINE mvTextLeft #-}
               
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
    
    -- Continuing Token: A continuing token spans more than one
    -- TcfText without break.
    
    -- Case 1: Second text is yust 1 char, like in "Hall" "o".
    tokenize' i (x1@(TcfText t1 _ _) : x2@(TcfText (c:[]) _ _) : xs)
      | (not $ null t1) && -- First text is non-empty.
        (not $ hasBreak t1) && -- First text is a token or less.
        (not $ isBreak c) -- Text 2 does not start with break.
      -- We do not create a token, but move letters from the second
      -- text to the first. This will be repeated for tokens that span
      -- several text nodes.
      = tokenize' i ((mvTextLeft x1 x2 1) ++ xs)

    -- Case 2, general case. Cases 1 is not covered by this because
    -- (t2':_) does not match (t2':[]).
    tokenize' i (x1@(TcfText t1 _ _) : x2@(TcfText t2@(t2':_) _ _) : xs)
      | (not $ null t1) && -- First text is non-empty.
        (not $ hasBreak t1) && -- First text is a token or less.
        (not $ isBreak t2') -- Text 2 does not start with break.
      -- We do not create a token, but move letters from the second
      -- text to the first. This will be repeated for tokens that span
      -- several text nodes.
      = tokenize' i ((mvTextLeft x1 x2 letters) ++ xs)

      -- More context needed for abbrev
      -- wrapping in words is for continuing over white space only nodes 
      | (not $ null t1) && (last t1) == '.'
      = tokenize' i ((mvTextLeft x1 x2 (spaces+fstWord)) ++ xs)

      where
        letters = length $ takeWhile (not . isBreak) t2
        spaces = length $ takeWhile (isSpace) t2
        fstWord = length $ takeWhile (not . isSpace) $ drop spaces t2
        
    -- Hyphenation At Linebreak:

    -- Drop linebreaks that were not preceded by a hyphen
    tokenize' i ((TcfLineBreak):xs) = tokenize' i xs
    tokenize' i (x@(TcfText t _ _):TcfLineBreak:xs)
      | (not $ null t) && (not $ ((last t) `elem` hyphens))
      = tokenize' i (x:xs)
    
    -- Hyphenation Case 1: Lines are enclosed in line-elements,
    -- e.g. <TEI:l>, and these lines are pretty printed each in one
    -- line of the xml-file. This results in i) a TcfText with
    -- trailing hyphen, ii) a TcfText with a newline and maybe other
    -- white space, iii) a TcfLineBreak, iv) the next text or
    -- structure elements. Here we just drop the white space (ii), so
    -- that this case is turned into Case 2.
    tokenize' i (x1@(TcfText t1 _ _):(TcfText t2 _ _):(TcfLineBreak):xs)
      | (not $ null t1) && -- first text must be non-empty
        (not $ hasBreak t1) && -- may there be trailing whitespace?
        ((last t1) `elem` hyphens) && -- trailing hyphen in t1
        (null $ dropWhile isSpace t2) -- drop white space node after linebreak
      = tokenize' i (x1:(TcfLineBreak):xs)

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
    tokenize' i (x1@(TcfText t1 _ _):(TcfLineBreak):x2@(TcfText t2 _ _):xs)
      | (not $ null t1) && -- first text must be non-empty
        (not $ hasBreak t1) && -- first text is a word. May there be trailing whitespace?
        ((last t1) `elem` hyphens) -- trailing hyphen in first text
      = tokenize' i ((TcfText
                      ((init t1)++(take letters $ drop spaces t2))
                      (getTextOffset x1)
                      ((init (getSrcCharOffsets x1)++(take letters $ drop spaces $ getSrcCharOffsets x2))))
                     :
                     (TcfText
                      (drop (letters+spaces) t2)
                      (shiftTextPosition (getTextOffset x2) (letters+spaces))
                      (drop (letters+spaces) $ getSrcCharOffsets x2))
                     :xs)
        --((mvTextLeft x1 x2 (letters+spaces) init (take letters . drop spaces)) ++ xs)
      where
        spaces = length $ takeWhile isSpace t2
        letters = length $ takeWhile (not . isBreak) (drop spaces t2)

    -- Drop zero length text node.
    tokenize' i ((TcfText "" _ _):xs)
      = tokenize' i xs
    -- Drop zero length text node 1 position ahead. This rests from
    -- continuing token, case 1.
    tokenize' i (x:(TcfText "" _ _):xs)
      = tokenize' i (x:xs)

    -- Make tokens by eating a single TcfText element.
    tokenize' i (x@(TcfText tx@(t:ts) _ _):xs)
      -- Drop heading space
      | isSpace t
      = tokenize' i ((mkText x spaces) : xs)

      -- Abbreviations
      
      -- [a-zA-Z]\.: one letter followed be dot
      | mk1CharAbbrevP && (isLetter t) && (not $ null ts) && (head ts) == '.'
      = (mkToken (t:".") i x 0)
        : (tokenize' (i+1) ((mkText x 2) : xs))
      -- abbrev: next word lower case
      | wdsLen >= 2 && (last fstWd) == '.' && (isLower $ head sndWd)
      = (mkToken fstWd i x 0)
        : (tokenize' (i+1) ((mkText x fstWdLen) : xs))
      -- Date (i): day. month. year[. -> sentence boundary]. We
      -- generate 3 Tokens to get the sentence boundary right, which
      -- is left for the next call of tokenize'. FIXME: this is german
      -- date format.
      | wdsLen >= 3 &&
          (isNumDay fstWd) &&             -- numeric day
          ((isNumMonth sndWd) ||          -- numeric month
           (isLitMonthAbbrev sndWd)) &&   -- or literal month
          (all isDigit $ take thrdWdLen' thrdWd)   -- numeric year
      = (mkToken fstWd i x 0)
        : (mkToken sndWd (i+1) x sndWdStart)
        : (mkToken (take thrdWdLen' thrdWd) (i+2) x thrdWdStart)
        : (tokenize' (i+3) ((mkText x (thrdWdStart+thrdWdLen')) : xs))
      -- Date (ii): day. month. We generate 2 tokens. FIXME: german date format.
      | wdsLen >= 2 &&
          (isNumDay fstWd) &&            -- numeric day
          ((isNumMonth sndWd) ||         -- numeric month
           (isLitMonthAbbrev sndWd))     -- literal month
      = (mkToken fstWd i x 0)
        : (mkToken sndWd (i+1) x sndWdStart)
        : (tokenize' (i+2) ((mkText x (sndWdStart+sndWdLen)) : xs))
      -- Date (iii): day. literal month not abbreviated. We generate 2
      -- tokens. FIXME: german date format.
      | wdsLen >= 2 &&
          (isNumDay fstWd) &&      -- numeric day
          ((take sndWdLen' sndWd) `elem` months)     -- literal month
      = (mkToken fstWd i x 0)
        : (mkToken (take sndWdLen' sndWd) (i+1) x sndWdStart)
        : (tokenize' (i+2) ((mkText x (sndWdStart+sndWdLen')) : xs))
      -- Date (iv): literal month abbreviated, without day. We
      -- generate 1 Token.
      | isLitMonthAbbrev fstWd
      = (mkToken fstWd i x 0)
        : (tokenize' (i+1) ((mkText x fstWdLen) : xs))

      -- [0-9]\.: one digit followed by dot
      | mkSingleDigitOrdinalP && (isDigit t) && (not $ null ts) && (head ts) == '.'
      = (mkToken (t:".") i x 0)
        : (tokenize' (i+1) ((mkText x 2) : xs))

      -- Abbreviation from config
      | isAbbrev fstWd
      = (mkToken fstWd i x 0)
        : (tokenize' (i+1) ((mkText x fstWdLen) : xs))

      -- Punctuation token
      | isPunctuation t
      = (mkToken (t:[]) i x 0)
        : (tokenize' (i+1) ((mkText x 1) : xs))
      -- Ordinary word token. Letters here means letters, digits,
      -- marks etc., but neither spaces nor punctuation.
      | otherwise
      = (mkToken (take letters tx) i x 0)
        : (tokenize' (i+1) ((mkText x letters) : xs ))
      where
        spaces = length $ takeWhile isSpace tx
        letters = length $ takeWhile (not . isBreak) tx
        wds = words tx -- list of words, but punctuation is constituent of word
        wdsLen = length wds -- count of words
        fstWd = head wds -- first of the words
        fstWdLen = length fstWd -- length of first word
        sndWd = wds !! 1 -- second of the words
        sndWdLen = length sndWd
        sndWdLen' = length $ takeWhile (not . isBreak) sndWd -- no dot
        thrdWd = wds !! 2
        thrdWdLen' = length $ takeWhile (not . isBreak) thrdWd -- no dot
        sndWdStart = nthWordStart isBreak 1 tx
        thrdWdStart = nthWordStart isBreak 2 tx
