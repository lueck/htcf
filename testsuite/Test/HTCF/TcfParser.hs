{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.HTCF.TcfParser where

import Test.Framework
import Test.QuickCheck.Monadic
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runXIOState, initialState)
import Data.Maybe

import HTCF.TcfParserTypeDefs
import HTCF.TcfParser
import HTCF.LineOffsets
import qualified HTCF.PosParser.ReadDocument as RD (readDocument)

sampleFile = "doc/examples/heine_broken.TEI-P5.xml"

-- | helper function
parseWithoutPos :: FilePath -> IO [TcfElement]
parseWithoutPos fName =
  runXIOState (initialState [])
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

-- | Testing for exact number of tcf text elements. This asserts that
-- mkTcfElement produces elements regardless of presence of source
-- positions. FIXME: This may break with implementation of hxt's
-- XmlParsec.
test_tcfTextWithoutPos = do
  parsedWithoutPos <- parseWithoutPos sampleFile
  assertEqual
    98
    (length $ filter isTcfText parsedWithoutPos)

-- | Comparing number of tcf text elements parsed with and without
-- source offsets. FIXME: This may break with implementation of hxt's
-- XmlParsec.
test_tcfCompareTexts = do
  parsedWithoutPos <- parseWithoutPos sampleFile
  parsedWithPos <- parseWithPos sampleFile
  assertBool
    -- at least there a 10 text elements more when positions are
    -- parsed, because 5 char refs cause 15 instead of 5.--There may
    -- be more than 10 en plus, because of canonicalization.
    ((lenText parsedWithPos) >= (10 + (lenText parsedWithoutPos)))
    where
      lenText = length . filter isTcfText
