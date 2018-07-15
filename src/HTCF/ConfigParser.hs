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

-- * Parsing the xml config file.

-- | Returns the config defined in a XML config file.
runConfigParser :: FilePath -> IO [Config]
runConfigParser fname = do
  exists <- doesFileExist fname
  progName <- getProgName
  if exists then
    do { results <- runX (readDocument [withValidate no] fname >>>
                          propagateNamespaces //>
                          hasName "config" >>>
                          multi parseConfig)
       ; return results}
    else
    do { hPutStrLn stderr (progName ++ ": No config file found. Using default config")
       ; return [] }

-- | An arrow for parsing the config file. Cf. implementation of
-- 'runConfigParser' for usage.
parseConfig :: IOSArrow XmlTree Config
parseConfig =
  pcTextRoot <+>
  pcDroppedTreeSimple <+>
  pcLinebreak <+>
  pcHyphen <+>
  pcNoBreak <+>
  pcAbbrev1CharToken <+>
  pcSingleDigitOrdinal <+>
  pcMonth <+>
  pcTcfRootNamespace <+>
  pcTcfTextCorpusNamespace <+>
  pcTcfMetadataNamespace <+>
  pcTcfIdBase <+>
  pcTcfTokenIdPrefix <+>
  pcTcfSentenceIdPrefix

-- * Parsing the XML config file

-- | Arrows for parsing special configuration aspects are all prefixed
-- with pc which stands for parseConfig.

pcTextRoot :: IOSArrow XmlTree Config
pcTextRoot =
  hasName "textRoot" >>>
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (TextRoot . (uncurry mkNsName))

pcDroppedTreeSimple :: IOSArrow XmlTree Config
pcDroppedTreeSimple =
  hasName "droppedTree" >>> getChildren >>>
  hasName "simpleElement" >>>
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (DroppedTree . (uncurry mkNsName))

pcLinebreak :: IOSArrow XmlTree Config
pcLinebreak =
  hasName "tokenizer" >>> getChildren >>>
  hasName "linebreak" >>>
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (LineBreak . (uncurry mkNsName))

pcHyphen :: IOSArrow XmlTree Config
pcHyphen =
  hasName "tokenizer" >>> getChildren >>>
  hasName "hyphen" >>>
  getAttrValue0 "char" >>>
  arr (Hyphen . head)

pcNoBreak :: (ArrowXml a) => a XmlTree Config
pcNoBreak =
  hasName "tokenizer" >>> getChildren >>>
  hasName "noBreak" >>>
  getAttrValue0 "char" >>>
  arr (NoBreak . head)

pcMonth :: IOSArrow XmlTree Config
pcMonth =
  hasName "tokenizer" >>> getChildren >>>
  hasName "month" >>> getChildren >>>
  isText >>> getText >>>
  arr (Month)

pcAbbrev1CharToken :: IOSArrow XmlTree Config
pcAbbrev1CharToken =
  hasName "tokenizer" >>> getChildren >>>
  hasName "abbrev1Char" >>>
  getAttrValue0 "abbrev" >>>
  arr (Abbrev1CharToken . (== "True"))

pcSingleDigitOrdinal :: (ArrowXml a) => a XmlTree Config
pcSingleDigitOrdinal =
  hasName "tokenizer" >>> getChildren >>>
  hasName "singleDigitOrdinal" >>>
  getAttrValue0 "ordinal" >>>
  arr (SingleDigitOrdinal . (== "True"))

pcTcfRootNamespace :: (ArrowXml a) => a XmlTree Config
pcTcfRootNamespace =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfRootNamespace" >>>
  getAttrValue "namespace" >>>
  arr (TcfRootNamespace . _cfg_onNull _cfg_tcfRootNamespace)

pcTcfTextCorpusNamespace :: IOSArrow XmlTree Config
pcTcfTextCorpusNamespace =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfTextCorpusNamespace" >>>
  getAttrValue "namespace" >>>
  arr (TcfTextCorpusNamespace . _cfg_onNull _cfg_tcfTextCorpusNamespace)

pcTcfMetadataNamespace :: IOSArrow XmlTree Config
pcTcfMetadataNamespace =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfMetadataNamespace" >>>
  getAttrValue "namespace" >>>
  arr (TcfMetadataNamespace . _cfg_onNull _cfg_tcfMetadataNamespace)

pcTcfIdBase :: IOSArrow XmlTree Config
pcTcfIdBase =
  hasName "tcf" >>> getChildren >>>
  hasName "idBase" >>>   getAttrValue "base" >>>
  arr (TcfIdBase . fromMaybe _cfg_tcfIdBase . fmap fst . C.readInt . C.pack) 

pcTcfTokenIdPrefix :: IOSArrow XmlTree Config
pcTcfTokenIdPrefix =
  hasName "tcf" >>> getChildren >>>
  hasName "tokenIdPrefix" >>> getAttrValue "prefix" >>>
  arr (TcfTokenIdPrefix . _cfg_onNull _cfg_tcfTokenIdPrefix)

pcTcfSentenceIdPrefix :: IOSArrow XmlTree Config
pcTcfSentenceIdPrefix =
  hasName "tcf" >>> getChildren >>>
  hasName "sentenceIdPrefix" >>> getAttrValue "prefix" >>>
  arr (TcfSentenceIdPrefix . _cfg_onnull _cfg_tcfSentenceIdPrefix)

-- | Helper function
defaultOnNull :: [a] -> [a] -> [a]
defaultOnNull deflt [] = deflt
defaultOnNull _ (x:xs) = x:xs
