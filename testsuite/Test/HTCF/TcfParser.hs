{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.HTCF.TcfParser where

import Test.Framework
import Test.QuickCheck.Monadic
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runXIOState, initialState)
import Data.Maybe

import HTCF.ConfigParser
import HTCF.ArrowXml
import HTCF.TcfParserTypeDefs
import HTCF.TcfParser
import HTCF.LineOffsets
import qualified HTCF.PosParser.ReadDocument as RD (readDocument)

--sampleFile = "doc/examples/heine_broken.TEI-P5.xml"
sampleFile = "doc/examples/kant_aufklaerung_1784.TEI-P5.xml"

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

-- | Assert that the lengths of the text and of the charPos fields of
-- a TcfText record equal after parsing. If they were not, the source
-- positions would fail.
prop_charPosLength :: Positive Int -> Property
prop_charPosLength (Positive i) = monadicIO $ do
  parsed <- run (parseWithPos sampleFile)
  let
    tcfTexts = filter isTcfText parsed
    txt = tcfTexts !! (mod i $ length tcfTexts)
  assert ((length $ getTcfText txt) == (length $ getSrcCharOffsets txt))

-- | Like prop_charPosLength, but without source positions but Nothing
-- instead.
prop_charPosLengthNoPos :: Positive Int -> Property
prop_charPosLengthNoPos (Positive i) = monadicIO $ do
  parsed <- run (parseWithoutPos sampleFile)
  let
    tcfTexts = filter isTcfText parsed
    txt = tcfTexts !! (mod i $ length tcfTexts)
  assert ((length $ getTcfText txt) == (length $ getSrcCharOffsets txt))

-- | Assert that the source offsets of the parsed structure elements
-- are correct by asserting < and > in the source. This test asserts the
-- start tag.
prop_assertStartTagAtStructurePos :: Positive Int -> Property
prop_assertStartTagAtStructurePos (Positive i) = monadicIO $ do
  parsed <- run (parseWithPos sampleFile)
  src <- run (readFile sampleFile)
  let
    tcfStructures = filter isTcfStructure parsed
    structure = tcfStructures !! (mod i $ length tcfStructures) -- random
    start = getSrcStartPos structure
    startChar = src !! (fromMaybe 4 $ start)
  monitor (counterexample
           ("Failing start tag: Found \'" ++
            (startChar:[]) ++
            "\' instead of \'<\' at offset " ++
            (show start) ++
            " of " ++
            sampleFile))
  assert ((startChar == '<') || (isNothing start))

-- | Assert that the source offsets of the parsed structure elements
-- are correct by asserting < and > in the source. This test asserts
-- the end tag.
prop_assertEndTagAtStructurePos :: Positive Int -> Property
prop_assertEndTagAtStructurePos (Positive i) = monadicIO $ do
  parsed <- run (parseWithPos sampleFile)
  src <- run (readFile sampleFile)
  let
    tcfStructures = filter isTcfStructure parsed
    structure = tcfStructures !! (mod i $ length tcfStructures) -- random
    end = getSrcEndPos structure
    endChar = src !! (fromMaybe 0 $ end)
  monitor (counterexample
           ("Failing end tag: Found \'" ++
            (endChar:[]) ++
            "\' instead of \'>\' at offset " ++
            (show end) ++
            " of " ++
            sampleFile))
  assert ((endChar == '>') || (isNothing end))


{-
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
-}
