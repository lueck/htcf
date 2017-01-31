module HTCF.Tokenizer
  ( Token (..)
  , tokenize
  , getToken
  , nthWordStart
  , dropNWords
  , isNumDay
  , isNumMonth
  , isRealAbbrev
  ) where

import Data.Char
import Data.List
import Data.Maybe

import HTCF.LayerTypeDefs
import HTCF.Position
import HTCF.TcfParserTypeDefs
import HTCF.ConfigParser

getToken :: Token -> String
getToken (Token t _ _ _ _ _) = t

isNumDay :: String -> Bool
isNumDay (d:'.':[]) = isDigit d
isNumDay ('0':d:'.':[]) = isDigit d
isNumDay ('1':d:'.':[]) = isDigit d
isNumDay ('2':d:'.':[]) = isDigit d
isNumDay ('3':d:'.':[]) = d == '0' || d == '1'
isNumDay _ = False

isNumMonth :: String -> Bool
isNumMonth (d:'.':[]) = isDigit d
isNumMonth ('0':d:'.':[]) = isDigit d
isNumMonth ('1':d:'.':[]) = d == '0' || d == '1' || d == '2'
isNumMonth _ = False

-- | Returns true, if the test string is a real subsequence of an
-- abbreviation string, i.e. it is shorter than the matching
-- abbreviation string.
isRealAbbrev :: [String] -> String -> Bool
isRealAbbrev [] _ = False
isRealAbbrev _ [] = False
isRealAbbrev abbrs str
  | (last str == '.') && (not $ null abbr) = length str <= (length $ head abbr)
  | otherwise = False
  where
    strWithoutDot = init str
    abbr = filter (isSubsequenceOf strWithoutDot) abbrs


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
      | c `elem` (getNoBreaks cfg) = False -- non-breaking characters from config
      | otherwise = isSpace c || isPunctuation c -- spaces and punctuation do
    hasBreak :: String -> Bool
    -- FIXME: 'any' from Data.List may be faster.
    hasBreak s = isJust $ find isBreak s

    isAbbrev :: String -> Bool
    -- the abbreviations from the config have not dot.
    isAbbrev str = str `elem` (map (++ ".") $ getAbbreviations cfg)

    isLitMonthAbbrev :: String -> Bool
    isLitMonthAbbrev = isRealAbbrev (getMonths cfg)


    mkToken :: String -- the token
            -> Int -- the token ID
            -> TextPosition -- the position in text layer
            -> XmlPosition -- the position in source
            -> Int -- offset in current string (text node)
            -> Token
    mkToken tok idd tOffset xOffset 0 =
      (Token tok (Just idd)
         (Just tOffset)
         (Just $ shiftTextPosition tOffset $ length tok)
         (Just xOffset)
         (Just $ shiftXmlPosition xOffset $ length tok))
    mkToken tok idd tOffset xOffset offset =
      (Token tok (Just idd)
         (Just $ shiftTextPosition tOffset offset)
         (Just $ shiftTextPosition tOffset $ offset + length tok)
         (Just $ shiftXmlPosition xOffset offset)
         (Just $ shiftXmlPosition xOffset $ offset + length tok))

    mkText :: String         -- the old text
           -> Int            -- number of chars to drop from the old text (text node)
           -> TextPosition   -- old position in text layer
           -> XmlPosition    -- old position in xml source
           -> TcfElement
    mkText txt n tOffset xOffset =
      (TcfText
       (drop n txt)
       (shiftTextPosition tOffset n+1)
       (shiftXmlPosition xOffset n+1))

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
                      (shiftXmlPosition sOffset2 letters))
                     : xs)
      -- More context needed for abbrev
      -- wrapping in words is for continuing over white space only nodes 
      | (not $ null t1) && (last t1) == '.'
      = tokenize' i ((TcfText
                      (t1 ++ take (spaces+fstWord) (t2:t2s))
                      tOffset1
                      sOffset1)
                     :
                     (TcfText
                      (drop (spaces+fstWord) (t2:t2s))
                      (shiftTextPosition tOffset2 (spaces+fstWord))
                      (shiftXmlPosition sOffset2 (spaces+fstWord)))
                     : xs)
      where
        letters = length $ takeWhile (not . isBreak) (t2:t2s)
        spaces = length $ takeWhile (isSpace) (t2:t2s)
        fstWord = length $ takeWhile (not . isSpace) $ drop spaces (t2:t2s)
        
    -- Hyphenation At Linebreak:

    -- Drop linebreaks that were not preceded by a hyphen
    tokenize' i ((TcfLineBreak):xs) = tokenize' i xs
    tokenize' i ((TcfText t tOffset sOffset):TcfLineBreak:xs)
      | (not $ null t) && (not $ ((last t) `elem` (getHyphens cfg)))
      = tokenize' i ((TcfText t tOffset sOffset):xs)
    
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

    -- Drop zero length text node.
    tokenize' i ((TcfText "" _ _):xs)
      = tokenize' i xs

    -- Make tokens by eating a single TcfText element.
    tokenize' i ((TcfText x@(t:ts) tOffset sOffset):xs)
      -- Drop heading space
      | isSpace t
      = tokenize' i ((mkText (t:ts) spaces tOffset sOffset) : xs)

      -- Abbreviations
      
      -- [a-zA-Z]\.: one letter followed be dot
      | (isLetter t) && (not $ null ts) && (head ts) == '.' &&
        (abbrev1CharTokenP cfg)
      = (mkToken (t:".") i tOffset sOffset 0)
        : (tokenize' (i+1) ((mkText (t:ts) 2 tOffset sOffset) : xs))
      -- abbrev: next word lower case
      | length wds >= 2 && (last $ head wds) == '.' && (isLower $ head $ wds !! 1)
      = (mkToken (head wds) i tOffset sOffset 0)
        : (tokenize' (i+1) ((mkText (t:ts) fstWdLen tOffset sOffset) : xs))
      -- Date (i): day. month. year[. -> sentence boundary]. We
      -- generate 3 Tokens to get the sentence boundary right, which
      -- is left for the next call of tokenize'. FIXME: this is german
      -- date format.
      | length wds >= 3 &&
          (isNumDay $ head wds) &&       -- numeric day
          ((isNumMonth $ wds !! 1) ||    -- numeric month
           (isLitMonthAbbrev $ wds !! 1)) &&   -- literal month
          (all isDigit $ take thrdWdLen' $ wds !! 2)   -- numeric year
      = (mkToken (head wds) i tOffset sOffset 0)
        : (mkToken (wds !! 1) (i+1) tOffset sOffset sndWdStart)
        : (mkToken (take thrdWdLen' (wds !! 2)) (i+2) tOffset sOffset thrdWdStart)
        : (tokenize' (i+3) ((mkText (t:ts) (thrdWdStart+thrdWdLen') tOffset sOffset) : xs))
      -- Date (ii): day. month. We generate 2 tokens. FIXME: german date format.
      | length wds >= 2 &&
          (isNumDay $ head wds) &&      -- numeric day
          ((isNumMonth $ wds !! 1) ||   -- numeric month
           (isLitMonthAbbrev $ wds !! 1))     -- literal month
      = (mkToken (head wds) i tOffset sOffset 0)
        : (mkToken (wds !! 1) (i+1) tOffset sOffset sndWdStart)
        : (tokenize' (i+2) ((mkText (t:ts) (sndWdStart+sndWdLen) tOffset sOffset) : xs))
      -- Date (iii): day. literal month not abbreviated. We generate 2
      -- tokens. FIXME: german date format.
      | length wds >= 2 &&
          (isNumDay $ head wds) &&      -- numeric day
          ((take sndWdLen' $ wds !! 1) `elem` (getMonths cfg))     -- literal month
      = (mkToken (head wds) i tOffset sOffset 0)
        : (mkToken (take sndWdLen' (wds !! 1)) (i+1) tOffset sOffset sndWdStart)
        : (tokenize' (i+2) ((mkText (t:ts) (sndWdStart+sndWdLen') tOffset sOffset) : xs))
      -- Date (iv): literal month abbreviated, without day. We
      -- generate 1 Token.
      | isLitMonthAbbrev $ head wds
      = (mkToken (head wds) i tOffset sOffset 0)
        : (tokenize' (i+1) ((mkText (t:ts) fstWdLen tOffset sOffset) : xs))

      -- [0-9]\.: one digit followed by dot
      | (isDigit t) && (not $ null ts) && (head ts) == '.' &&
        (singleDigitOrdinalP cfg)
      = (mkToken (t:".") i tOffset sOffset 0)
        : (tokenize' (i+1) ((mkText x 2 tOffset sOffset) : xs))

      -- Abbreviation from config
      | isAbbrev $ head wds
      = (mkToken (head wds) i tOffset sOffset 0)
        : (tokenize' (i+1) ((mkText (t:ts) fstWdLen tOffset sOffset) : xs))

      -- Punctuation token
      | isPunctuation t
      = (mkToken (t:[]) i tOffset sOffset 0)
        : (tokenize' (i+1) ((mkText (t:ts) 1 tOffset sOffset) : xs))
      -- Ordinary word token. Letters here means letters, digits,
      -- marks etc., but neither spaces nor punctuation.
      | otherwise
      = (mkToken (take letters (t:ts)) i tOffset sOffset 0)
        : (tokenize' (i+1) ((mkText (t:ts) letters tOffset sOffset) : xs ))
      where
        spaces = length $ takeWhile isSpace (t:ts)
        letters = length $ takeWhile (not . isBreak) (t:ts)
        wds = words (t:ts) -- list of words, but punctuation is constituent of word
        fstWdLen = length $ head wds
        sndWdLen = length (wds !! 1)
        sndWdLen' = length $ takeWhile (not . isBreak) (wds !! 1) -- no dot
        thrdWdLen' = length $ takeWhile (not . isBreak) (wds !! 2) -- no dot
        sndWdStart = nthWordStart isBreak 1 (t:ts)
        thrdWdStart = nthWordStart isBreak 2 (t:ts)
