{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Text.XML.TCF.Arrow.ArrowXml where

import Test.Framework

import Text.XML.HXT.Core

import Text.XML.TCF.Arrow.ArrowXml

fileA = "testsuite/Test/Text/XML/TCF/Arrow/file-a.xml"

test_nameIn = do
  results <- runX (readDocument [withValidate no] fileA >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   getChildren >>>
                   isElem >>> nameIn ["pre", "code"] >>>
                   getChildren >>>
                   getText)
  assertEqual
    ["main = putStrLn \"hello world\"\n",
     "$ runghc hello1.hs",
     "Prelude> :load hello1.hs\n*Main> main\n",
     "main = interact (const \"hello world\\n\")\n"]
    results

test_qNameIn = do
  results <- runX (readDocument [withValidate no] fileA >>>
                   propagateNamespaces >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   getChildren >>>
                   --isElem >>> hasQName (mkQName "xx" "pre" "http://www.w3.org/1999/xhtml") >>>
                   --isElem >>> hasQName (mkNsName "http://www.w3.org/1999/xhtml" "pre") >>>
                   isElem >>> qNameIn [(mkQName "xx" "pre" "http://www.w3.org/1999/xhtml")
                                      ,(mkQName "xx" "code" "http://www.w3.org/1999/xhtml")] >>>
                   getChildren >>>
                   getText)
  assertEqual
    ["main = putStrLn \"hello world\"\n",
     "$ runghc hello1.hs",
     "Prelude> :load hello1.hs\n*Main> main\n",
     "main = interact (const \"hello world\\n\")\n"]
    results

-- stripName may be put anywhere
test_stripName = do
  results <- runX (readDocument [withValidate no] fileA >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   stripName "p" >>> -- strip <p>-elements
                   getChildren >>>
                   isElem >>>
                   getChildren >>>
                   getText)
  resultS <- runX (readDocument [withValidate no] fileA >>>
                   stripName "p" >>> -- strip <p>-elements
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   getChildren >>>
                   isElem >>>
                   getChildren >>>
                   getText)
  assertEqual
    ["Test",
     " is for testing.",
     "main = putStrLn \"hello world\"\n",
     "$ runghc hello1.hs",
     "Prelude> :load hello1.hs\n*Main> main\n",
     "main = interact (const \"hello world\\n\")\n"]
    results
  assertEqual
    ["Test",
     " is for testing.",
     "main = putStrLn \"hello world\"\n",
     "$ runghc hello1.hs",
     "Prelude> :load hello1.hs\n*Main> main\n",
     "main = interact (const \"hello world\\n\")\n"]
    resultS

test_stripNames = do
  results <- runX (readDocument [withValidate no] fileA >>>
                   stripNames ["pre","code"] >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   getChildren >>>
                   isElem >>>
                   getChildren >>>
                   getText)
  resultS <- runX (readDocument [withValidate no] fileA >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   stripNames ["pre","code"] >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   getChildren >>>
                   isElem >>>
                   getChildren >>>
                   getText)
  assertEqual
    ["Test",
     " is for testing.",
     "This is first paragraph.",
     "To run it, enter this at a shell prompt:",
     "This is third paragraph.",
     "This is the fourth paragraph."]
    results
  assertEqual
    ["Test",
     " is for testing.",
     "This is first paragraph.",
     "To run it, enter this at a shell prompt:",
     "This is third paragraph.",
     "This is the fourth paragraph."]
    resultS

test_stripQNames = do
  results <- runX (readDocument [withValidate no] fileA >>>
                   propagateNamespaces >>>
                   stripQNames [(mkNsName "pre" "http://www.w3.org/1999/xhtml"),
                                (mkNsName "code" "http://www.w3.org/1999/xhtml")] >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   getChildren >>>
                   isElem >>>
                   getChildren >>>
                   getText)
  assertEqual
    ["Test",
     " is for testing.",
     "This is first paragraph.",
     "To run it, enter this at a shell prompt:",
     "This is third paragraph.",
     "This is the fourth paragraph."]
    results

