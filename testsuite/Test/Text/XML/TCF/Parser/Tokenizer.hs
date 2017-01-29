{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.XML.TCF.Parser.Tokenizer where

import Test.Framework
import Text.XML.HXT.Core

import Text.XML.TCF.Parser.Tokenizer
import Text.XML.TCF.Parser.ConfigParser
import Text.XML.TCF.Parser.TcfElement

test_ordinaryWord = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "Hallo Welt" 1 1)]

test_punctuation = do
  assertEqual
    ["Hallo", "Welt", "!"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "Hallo Welt!" 1 1)]

test_emptyString = do
  assertEqual
    []
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "" 1 1)]

test_continuingWord = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "Hal" 1 1),
              (TcfText "lo Welt" 4 7)]

test_continuingWordOverStruct = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "Hal" 1 1),
              (TcfStructure (mkNsName "b" "http://www.w3.org/1999/xhtml") 4 10 4 16),
              (TcfText "lo Welt" 4 7)]

test_hyphenCase1 = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(TcfText "Hal-" 1 1),
              (TcfText "\n\t " 4 7),
              (TcfLineBreak),
              (TcfText "lo Welt" 10 17)]

test_hyphenCase1WithWhiteSpace = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(TcfText "Hal-" 1 1),
              (TcfText "\n\t " 4 7),
              (TcfLineBreak),
              (TcfText "\n\t\t lo Welt" 10 17)]

test_hyphenCase2 = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(TcfText "Hal-" 1 1),
              (TcfLineBreak),
              (TcfText "lo Welt" 4 7)]

test_hyphenCase2WithWhiteSpace = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(TcfText "Hal-" 1 1),
              (TcfLineBreak),
              (TcfText "\n\tlo Welt" 4 7)]

{- -- FIXME!
test_hyphenCase2WithWhiteSpaceContinued = do
  assertEqual
    ["Hallo", "Welt"]
    (map getToken (tokenize [(Hyphen '-')] parsed))
  where
    parsed = [(TcfText "Hal-" 1 1),
              (TcfLineBreak),
              (TcfText "\n\tl" 7 12),
              (TcfStructure (mkNsName "b" "http://www.w3.org/1999/xhtml") 11 13 11 16), 
              (TcfText "o Welt" 4 7)]
-}

test_numDateOnly = do
  assertEqual
    ["31.", "10.", "1517"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31. 10. 1517" 1 1)]  

test_numDate = do
  assertEqual
    ["31.", "10.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31. 10. 1517 is a date." 1 1)]  

test_numDateSentence = do
  assertEqual
    ["31.", "10.", "1517", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31. 10. 1517." 1 1)]  

test_numDateContinued = do
  assertEqual
    ["31.", "10.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31." 1 1),
              (TcfText " 10. 1517 is a date." 4 7)]  
  
test_numDateLineBreak = do
  assertEqual
    ["31.", "10.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31." 1 1),
              (TcfLineBreak),
              (TcfText "\n10. 1517 is a date." 4 7)]  
  
test_numLitMonthDate = do
  assertEqual
    ["31.", "Okt.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31. Okt. 1517 is a date." 1 1)]  

test_numLitMonthDateLineBreak = do
  assertEqual
    ["31.", "Oktob.", "1517", "is", "a", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31." 1 1),
              (TcfLineBreak),
              (TcfText "\nOktob. 1517 is a date." 4 7)]  

test_litMonthOnly = do
  assertEqual
    ["im", "Okt.", "1517"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "im   Okt. 1517" 1 1)]

test_numLitMonthDateNoAbbrev = do
  assertEqual
    ["war", "am", "31.", "Oktober", ".", "1517", "war", "ein"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "war am 31. Oktober. 1517 war ein" 1 1)]  

test_invalidDay = do
  assertEqual
    ["41", ".", "Oktob.", "1517", "is", "no", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "41. Oktob. 1517 is no date." 1 1)]  

test_invalidNumMonth = do
  assertEqual
    ["31", ".", "13", ".", "1517", "is", "no", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31. 13. 1517 is no date." 1 1)]  

test_invalidLitMonth = do
  assertEqual
    ["31", ".", "Octop", ".", "1517", "is", "no", "date", "."]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "31. Octop. 1517 is no date." 1 1)]  


-- More abbreviations

test_abbrevNoSpaces = do
  assertEqual
    ["etwas", "Ausgedehntes", ",", "z.", "B.", "eine", "Wolke"]
    (map getToken (tokenize [] parsed))
  where
    parsed = [(TcfText "etwas Ausgedehntes, z.B. eine Wolke" 1 1)]  
