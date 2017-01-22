{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.XML.TCF.Parser.ConfigParser where

import Test.Framework
import Text.XML.HXT.Core

import Text.XML.TCF.Parser.ConfigParser

configFile = "testsuite/Test/Text/XML/TCF/Parser/config.xml"

test_runConfigParser = do
  results <- runConfigParser configFile
  assertEqual
    [(TextRoot (mkNsName "http://www.tei-c.org/ns/1.0" "text")),
     (DroppedTree (mkNsName "http://www.tei-c.org/ns/1.0" "fw")),
     (LineBreak (mkNsName "http://www.tei-c.org/ns/1.0" "lb")),
     (LineBreak (mkNsName "http://www.tei-c.org/ns/1.0" "l")),
     (Hyphen '-'),
     (Hyphen '\172')]
    results

test_getters = do
  results <- runConfigParser configFile
  
  assertEqual
    (Just $ mkNsName "http://www.tei-c.org/ns/1.0" "text")
    (getTextRoot results)

  assertEqual
    "-¬"
    (getHyphens results)

  assertEqual
    [(mkNsName "http://www.tei-c.org/ns/1.0" "lb")
    ,(mkNsName "http://www.tei-c.org/ns/1.0" "l")]
    (getLineBreaks results)

  assertEqual
    [(mkNsName "http://www.tei-c.org/ns/1.0" "fw")]
    (getDroppedTrees results)
