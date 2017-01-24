{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.XML.TCF.Parser.ConfigParser where

import Test.Framework
import Text.XML.HXT.Core

import Text.XML.TCF.Parser.ConfigParser

configFile = "testsuite/Test/Text/XML/TCF/Parser/config.xml"

test_runConfigParser = do
  results <- runConfigParser configFile
  assertEqual
    [(TextRoot (mkNsName "text" "http://www.tei-c.org/ns/1.0")),
     (DroppedTree (mkNsName "fw" "http://www.tei-c.org/ns/1.0")),
     (LineBreak (mkNsName "lb" "http://www.tei-c.org/ns/1.0")),
     (LineBreak (mkNsName "l" "http://www.tei-c.org/ns/1.0")),
     (Hyphen '-'),
     (Hyphen '\172'),
     (TcfTextCorpusNamespace "http://www.dspin.de/data/textcorpus")]
    results

test_getters = do
  results <- runConfigParser configFile
  
  assertEqual
    (Just $ mkNsName "text" "http://www.tei-c.org/ns/1.0")
    (getTextRoot results)

  assertEqual
    "-¬"
    (getHyphens results)

  assertEqual
    [(mkQName "prefix" "lb" "http://www.tei-c.org/ns/1.0")
    ,(mkQName "TEI" "l" "http://www.tei-c.org/ns/1.0")]
    (getLineBreaks results)

  assertEqual
    [(mkNsName "fw" "http://www.tei-c.org/ns/1.0")]
    (getDroppedTrees results)

  assertEqual
    "http://www.dspin.de/data/textcorpus"
    (getTcfTextCorpusNamespace results)

  assertEqual
    10
    (getTcfIdBase results)

  assertEqual
    '_'
    (getTcfIdPrefixDelimiter results)

  assertEqual
    2
    (getTcfIdPrefixLength results)

  assertEqual
    Length
    (getTcfIdUnprefixMethod results)

test_Setters = do
  let config = setTcfIdPrefixLength 10 []

  assertEqual
    10
    (getTcfIdPrefixLength config)
