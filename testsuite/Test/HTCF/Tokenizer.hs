{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.HTCF.Tokenizer where

import Test.Framework
import Test.QuickCheck.Monadic
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runXIOState, initialState)
import Data.Maybe

import HTCF.Tokenizer
import HTCF.ConfigParser
import HTCF.TcfParser
import HTCF.TcfParserTypeDefs
import HTCF.TokenLayer
import HTCF.LineOffsets
import qualified HTCF.PosParser.ReadDocument as RD (readDocument)

sampleFile = "doc/examples/kant_aufklaerung_1784.TEI-P5.xml"

monthsConfig = [(Month "Oktober")]

mkTcfTextSample :: String -> Int -> Int -> TcfElement
mkTcfTextSample t tOffset xOffset =
  (TcfText t tOffset (replicate (length t) (Nothing, Nothing)))

mkTcfStructureSample :: QName -> Int -> Int -> Int -> Int -> TcfElement
mkTcfStructureSample qName tS tE xS xE =
  (TcfStructure qName tS tE (Just xS) (Just xE))

-- * Testing offsets

-- | helper function
parse :: FilePath -> IO [TcfElement]
parse fName = runXIOState (initialState [])
              (readDocument [withValidate no] fName >>>
               propagateNamespaces >>>
               multi (mkTcfElement [])
              )

-- | helper function
parseWithPos :: FilePath -> IO [TcfElement]
parseWithPos fName = do
  lineOffsets <- runLineOffsetParser fName
  parsed <- runXIOState (initialState lineOffsets)
            (RD.readDocument [withValidate no
                             ,withCanonicalize no] fName >>>
             propagateNamespaces >>>
             multi (mkTcfElement [])
            )
  return parsed

-- | Testing offsets of token with randomized number
prop_textOffset :: Positive Int -> Property
prop_textOffset (Positive i) = monadicIO $ do
  parsed <- run (parse sampleFile)
  let
    textLayer = concatMap getTcfText parsed
    tokens = tokenize [] $ propagateOffsets parsed
    tok = tokens !! (mod i $ length tokens) -- randomized token number
    start = fromMaybe 0 $ getTokenStartTextPos tok
    end = fromMaybe 0 $ getTokenEndTextPos tok
    wd = getToken tok
  assert ((take (end-start+1) $ drop start textLayer) == wd)

-- | Testing source offsets with randomize token number
prop_srcOffset :: Positive Int -> Property
prop_srcOffset (Positive i) = monadicIO $ do
  parsed <- run (parseWithPos sampleFile)
  srcContents <- run (readFile sampleFile)
  let
    textLayer = concatMap getTcfText parsed
    tokens = tokenize [] $ propagateOffsets parsed
    tok = tokens !! (mod i $ length tokens) -- randomized token number
    start = fromMaybe 0 $ getTokenStartSrcPos tok
    end = fromMaybe 0 $ getTokenEndSrcPos tok
    wd = getToken tok
    -- -1 because first char is !! 0. Is this right? And is it
    -- -consistent with text offsets? FIXME?
    srcTxt = take (end-start+1) $ drop (start) srcContents
  assert ((srcContents !! start) `elem` ((head wd):"&"))  -- start char
  assert ((srcContents !! end) `elem` ((last wd):";"))  -- end char
  assert ((srcTxt == wd) || ('&' `elem` srcTxt) || ('<' `elem` srcTxt)) -- all chars

-- | Testing test offset with a fixed token number. This indicates,
-- that we have not systematically lost any tokens.
test_textOffsetFixed = do
  parsed <- parse sampleFile
  let textLayer = concatMap getTcfText parsed
      tokens = tokenize [] $ propagateOffsets parsed
      tok = tokens !! 3700
      start = fromMaybe 0 $ getTokenStartTextPos tok
      end = fromMaybe 0 $ getTokenEndTextPos tok
      wd = getToken tok
  assertEqual
    (take (end-start+1) $ drop start textLayer)
    wd

-- * Testing tokenization

test_ordinaryWord = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "Hallo Welt" 1 1)]

test_punctuation = do
  assertEqual
    ["Hallo", "Welt", "!"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "Hallo Welt!" 1 1)]

test_emptyString = do
  assertEqual
    []
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "" 1 1)]

test_continuingWord = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "Hal" 1 1),
              (mkTcfTextSample "lo Welt" 4 7)]

test_continuingWordChar = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "Hal" 1 1),
              (mkTcfTextSample "l" 4 4),
              (mkTcfTextSample "o Welt" 5 7)]

test_continuingWordCharFst = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "H" 1 1),
              (mkTcfTextSample "al" 2 2),
              (mkTcfTextSample "l" 4 4),
              (mkTcfTextSample "o Welt" 5 7)]

test_continuingWordOverStruct = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "Hal" 1 1),
              (mkTcfStructureSample (mkNsName "b" "http://www.w3.org/1999/xhtml") 4 10 4 16),
              (mkTcfTextSample "lo Welt" 4 7)]

test_hyphenCase1 = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(mkTcfTextSample "Hal-" 1 1),
              (mkTcfTextSample "\n\t " 4 7),
              (TcfLineBreak),
              (mkTcfTextSample "lo Welt" 10 17)]

test_hyphenCase1WithWhiteSpace = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(mkTcfTextSample "Hal-" 1 1),
              (mkTcfTextSample "\n\t " 4 7),
              (TcfLineBreak),
              (mkTcfTextSample "\n\t\t lo Welt" 10 17)]

test_hyphenCase2 = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(mkTcfTextSample "Hal-" 1 1),
              (TcfLineBreak),
              (mkTcfTextSample "lo Welt" 4 7)]

test_hyphenCase2WithWhiteSpace = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(mkTcfTextSample "Hal-" 1 1),
              (TcfLineBreak),
              (mkTcfTextSample "\n\tlo Welt" 4 7)]

test_hyphenCase2WithWhiteSpaceContinued = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(mkTcfTextSample "Hal-" 1 1),
              (TcfLineBreak),
              (mkTcfTextSample "\n\tl" 7 12),
              (mkTcfStructureSample (mkNsName "b" "http://www.w3.org/1999/xhtml") 11 13 11 16), 
              (mkTcfTextSample "o Welt" 4 7)]

-- * Date tests

test_numDateOnly = do
  assertEqual
    ["31.", "10.", "1517"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "31. 10. 1517" 1 1)]

test_numDate = do
  assertEqual
    ["31.", "10.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "31. 10. 1517 is a date." 1 1)]

test_numDateSentence = do
  assertEqual
    ["31.", "10.", "1517", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "31. 10. 1517." 1 1)]

test_numDateContinued = do
  assertEqual
    ["31.", "10.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "31." 1 1),
              (mkTcfTextSample " 10. 1517 is a date." 4 7)]

test_numDateLineBreak = do
  assertEqual
    ["31.", "10.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "31." 1 1),
              (TcfLineBreak),
              (mkTcfTextSample "\n10. 1517 is a date." 4 7)]

test_numLitMonthDate = do
  assertEqual
    ["31.", "Okt.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize monthsConfig parsed))
  where
    parsed = [(mkTcfTextSample "31. Okt. 1517 is a date." 1 1)]

test_numLitMonthDateLineBreak = do
  assertEqual
    ["31.", "Oktob.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize monthsConfig parsed))
  where
    parsed = [(mkTcfTextSample "31." 1 1),
              (TcfLineBreak),
              (mkTcfTextSample "\nOktob. 1517 is a date." 4 7)]

test_litMonthOnly = do
  assertEqual
    ["im", "Okt.", "1517"]
    (map getToken (tokenize monthsConfig parsed))
  where
    parsed = [(mkTcfTextSample "im   Okt. 1517" 1 1)]

test_numLitMonthDateNoAbbrev = do
  assertEqual
    ["war", "am", "31.", "Oktober", ".", "1517", "war", "ein"]
    (map getToken (tokenize monthsConfig parsed))
  where
    parsed = [(mkTcfTextSample "war am 31. Oktober. 1517 war ein" 1 1)]

test_invalidDay = do
  assertEqual
    ["41", ".", "Oktob.", "1517", "is", "no", "date", "."]
    (map getToken (tokenize monthsConfig parsed))
  where
    parsed = [(mkTcfTextSample "41. Oktob. 1517 is no date." 1 1)]

test_invalidNumMonth = do
  assertEqual
    ["31", ".", "13", ".", "1517", "is", "no", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "31. 13. 1517 is no date." 1 1)]

test_invalidLitMonth = do
  assertEqual
    ["31", ".", "Octop", ".", "1517", "is", "no", "date", "."]
    (map getToken (tokenize monthsConfig parsed))
  where
    parsed = [(mkTcfTextSample "31. Octop. 1517 is no date." 1 1)]


-- More abbreviations

test_abbrevNoSpaces = do
  assertEqual
    ["etwas", "Ausgedehntes", ",", "z.", "B.", "eine", "Wolke"]
    (map getToken (tokenize [(Abbrev1CharToken True)] parsed))
  where
    parsed = [(mkTcfTextSample "etwas Ausgedehntes, z.B. eine Wolke" 1 1)]

test_confAbbrevs = do
  assertEqual
    ["Prof.", "Rammler"]
    (map getToken (tokenize [(Abbreviation "Prof")] parsed))
  where
    parsed = [(mkTcfTextSample "Prof. Rammler" 1 1)]

test_confNoAbbrevs = do
  assertEqual
    ["Prof", ".", "Rammler"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(mkTcfTextSample "Prof. Rammler" 1 1)]
