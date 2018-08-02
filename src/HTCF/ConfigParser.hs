-- | A config file will be parsed by 'runConfigParser' into a list of
-- @['Config']@. Use 'getTextRoot', 'getHyphens', etc. to get an
-- aspect of the configuration from this list.

module HTCF.ConfigParser
  ( runConfigParser
  , parseConfig
  , pcTextRoot
  , pcDroppedTreeSimple
  , pcLinebreak
  , pcHyphen
  , pcAbbrev1CharToken
  , pcSingleDigitOrdinal
  , pcMonth
  , pcTcfTextCorpusNamespace
  , pcTcfIdBase
  , pcTcfTokenIdPrefix
  , pcTcfSentenceIdPrefix
  ) where

import Text.XML.HXT.Core
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import System.IO
import System.Directory
import System.Environment
import Control.Lens
import Data.Default.Class

import HTCF.Config

-- * Parsing the xml config file.

-- | Returns the config defined in a XML config file.
runConfigParser :: FilePath -> IO Config
runConfigParser fname = do
  exists <- doesFileExist fname
  progName <- getProgName
  if exists then
    do { results <- runX (readDocument [withValidate no] fname >>>
                          propagateNamespaces //>
                          hasName "config" >>>
                          single parseConfig)
       ; return $ head results}
    else
    do { hPutStrLn stderr (progName ++ ": No config file found. Using default config")
       ; return def }

-- | An arrow for parsing the config file. Cf. implementation of
-- 'runConfigParser' for usage.
parseConfig :: IOSArrow XmlTree Config
parseConfig =
  (first pcTextRoot) &&&
  (multi pcDroppedTreeSimple) &&&
  (multi pcLinebreak) &&&
  arr (\(textRoot, (dropped, lbreaks)) ->
         def
         & cfg_textRoot .~ textRoot
         & cfg_droppedTrees .~ dropped
         & cfg_lineBreaks .~ lbreaks)
  -- pcHyphen <+>
  -- pcNoBreak <+>
  -- pcAbbrev1CharToken <+>
  -- pcSingleDigitOrdinal <+>
  -- pcMonth <+>
  -- pcTcfRootNamespace <+>
  -- pcTcfTextCorpusNamespace <+>
  -- pcTcfMetadataNamespace <+>
  -- pcTcfIdBase <+>
  -- pcTcfTokenIdPrefix <+>
  -- pcTcfSentenceIdPrefix

-- * Parsing the XML config file

-- | Arrows for parsing special configuration aspects are all prefixed
-- with pc which stands for parseConfig.

pcTextRoot :: IOSArrow XmlTree QName
pcTextRoot =
  hasName "textRoot" >>>
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (uncurry mkNsName)

pcDroppedTreeSimple :: IOSArrow XmlTree QName
pcDroppedTreeSimple =
  hasName "droppedTree" >>> getChildren >>>
  hasName "simpleElement" >>>
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (uncurry mkNsName)

pcLinebreak :: IOSArrow XmlTree QName
pcLinebreak =
  hasName "tokenizer" >>> getChildren >>>
  hasName "linebreak" >>>
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (uncurry mkNsName)

pcHyphen :: IOSArrow XmlTree Char
pcHyphen =
  hasName "tokenizer" >>> getChildren >>>
  hasName "hyphen" >>>
  getAttrValue0 "char" >>>
  arr head

pcNoBreak :: (ArrowXml a) => a XmlTree Char
pcNoBreak =
  hasName "tokenizer" >>> getChildren >>>
  hasName "noBreak" >>>
  getAttrValue0 "char" >>>
  arr head

pcMonth :: IOSArrow XmlTree String
pcMonth =
  hasName "tokenizer" >>> getChildren >>>
  hasName "month" >>> getChildren >>>
  isText >>> getText

pcAbbrev1CharToken :: IOSArrow XmlTree Bool
pcAbbrev1CharToken =
  hasName "tokenizer" >>> getChildren >>>
  hasName "abbrev1Char" >>>
  getAttrValue0 "abbrev" >>>
  arr (== "True")

pcSingleDigitOrdinal :: (ArrowXml a) => a XmlTree Bool
pcSingleDigitOrdinal =
  hasName "tokenizer" >>> getChildren >>>
  hasName "singleDigitOrdinal" >>>
  getAttrValue0 "ordinal" >>>
  arr (== "True")

pcTcfRootNamespace :: (ArrowXml a) => a XmlTree String
pcTcfRootNamespace =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfRootNamespace" >>>
  getAttrValue "namespace"

pcTcfTextCorpusNamespace :: IOSArrow XmlTree String
pcTcfTextCorpusNamespace =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfTextCorpusNamespace" >>>
  getAttrValue "namespace"

pcTcfMetadataNamespace :: IOSArrow XmlTree String
pcTcfMetadataNamespace =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfMetadataNamespace" >>>
  getAttrValue "namespace"

pcTcfIdBase :: IOSArrow XmlTree Int
pcTcfIdBase =
  hasName "tcf" >>> getChildren >>>
  hasName "idBase" >>>   getAttrValue "base" >>>
  arr (fromMaybe (def^.cfg_tcfIdBase) . fmap fst . C.readInt . C.pack)

pcTcfTokenIdPrefix :: IOSArrow XmlTree String
pcTcfTokenIdPrefix =
  hasName "tcf" >>> getChildren >>>
  hasName "tokenIdPrefix" >>> getAttrValue "prefix"

pcTcfSentenceIdPrefix :: IOSArrow XmlTree String
pcTcfSentenceIdPrefix =
  hasName "tcf" >>> getChildren >>>
  hasName "sentenceIdPrefix" >>> getAttrValue "prefix"
