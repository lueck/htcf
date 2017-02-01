{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.HTCF.ConfigParser where

import Test.Framework
import Text.XML.HXT.Core

import HTCF.ConfigParser

configFile = "testsuite/Test/HTCF/config.xml"

test_runConfigParser = do
  results <- runConfigParser configFile
  assertElem
    (TextRoot (mkNsName "text" "http://www.tei-c.org/ns/1.0"))
    results

test_getTextRoot = do
  results <- runConfigParser configFile
  assertEqual
    (mkNsName "text" "http://www.tei-c.org/ns/1.0")
    (getTextRoot results)

test_getHyphens = do
  results <- runConfigParser configFile
  assertEqual
    "¬"
    (getHyphens results)

test_getLinebreaks = do
  results <- runConfigParser configFile
  assertEqual
    [(mkQName "prefix" "lb" "http://www.tei-c.org/ns/1.0")
    ,(mkQName "TEI" "l" "http://www.tei-c.org/ns/1.0")]
    (getLineBreaks results)

test_getDroppedTrees = do
  results <- runConfigParser configFile
  assertElem
    (mkNsName "fw" "http://www.tei-c.org/ns/1.0")
    (getDroppedTrees results)

test_getTcfRootNamespace = do
  results <- runConfigParser configFile
  assertEqual
    "http://www.dspin.de/data"
    (getTcfRootNamespace results)
  assertEqual
    "http://www.dspin.de/data"
    (getTcfRootNamespace [])  

test_getTcfTextCorpusNamespace = do
  results <- runConfigParser configFile
  assertEqual
    "http://www.dspin.de/data/textcorpus"
    (getTcfTextCorpusNamespace results)
  assertEqual
    "http://www.dspin.de/data/textcorpus"
    (getTcfTextCorpusNamespace [])

test_getTcfIdBase = do
  results <- runConfigParser configFile
  assertEqual
    10
    (getTcfIdBase results)

test_getTcfIdPrefixDelimiter = do
  results <- runConfigParser configFile
  assertEqual
    '_'
    (getTcfIdPrefixDelimiter results)

test_getTcfIdPrefixLength = do
  results <- runConfigParser configFile
  assertEqual
    2
    (getTcfIdPrefixLength results)

test_getTcfIdUnprefixMethod = do
  results <- runConfigParser configFile
  assertEqual
    Length
    (getTcfIdUnprefixMethod results)

test_getAbbrev1CharToken = do
  results <- runConfigParser configFile
  assertBool
    (abbrev1CharTokenP results)

test_getMonths = do
  results <- runConfigParser configFile
  assertElem
    "Jänner"
    (getMonths results)


-- * Tests for setters:

test_setTcfIdPrefixLength = do
  let config = setTcfIdPrefixLength 10 []

  assertEqual
    10
    (getTcfIdPrefixLength config)
