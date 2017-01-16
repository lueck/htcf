{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.XML.TCF.Parser.ConfigParser where

import Test.Framework
import Text.XML.HXT.Core

import Text.XML.TCF.Parser.ConfigParser (stripped)

configFile = "testsuite/Test/Text/XML/TCF/Parser/config.xml"

test_stripped = do
  results <- runX (stripped configFile)
  assertEqual
    [mkNsName "http://www.tei-c.org/ns/1.0" "fw"]
    results
