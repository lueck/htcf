-- | A config file will be parsed by 'runConfigParser' into a list of
-- @['Config']@. Use 'getTextRoot', 'getHyphens', etc. to get an
-- aspect of the configuration from this list.

module Text.XML.TCF.Parser.ConfigParser
  ( Config (..)
  , runConfigParser
  , parseConfig
  , getTextRoot
  , getHyphens
  , getLineBreaks
  , getDroppedTrees
  , getTcfTextCorpusNamespace
  , getTcfIdBase
  , getTcfIdPrefixDelimiter
  , defaultTcfTextCorpusNamespace
  , defaultTcfIdBase
  , defaultTcfIdPrefixDelimiter
  ) where

import Text.XML.HXT.Core
import qualified Data.ByteString.Char8 as C
import Data.Maybe

data Config =
  TextRoot QName                        -- ^ qname of parent node for text
  | DroppedTree                         -- ^ xml subtree to be dropped by the parser
  { qName :: QName }                    -- ^ qname of element node
  | LineBreak QName                     -- ^ qname of element
                                        -- resulting in a control sign
                                        -- for the tokenizer
  | Hyphen Char                         -- ^ hyphen character for tokenizer
  | TcfTextCorpusNamespace String       -- ^ the namespace of the
                                        -- TextCorpus tag of a TCF
                                        -- file
  | TcfIdBase Int                       -- ^ The base of the IDs used in a TCf file
  | TcfIdPrefixDelimiter Char           -- ^ The character that
                                        -- delimits the prefix from
                                        -- the number in a ID used in
                                        -- a TCF file
  deriving (Show, Eq)

-- | Default value for the <TextCorpus> element of a TCF file:
-- http://www.dspin.de/data/textcorpus
defaultTcfTextCorpusNamespace :: String
defaultTcfTextCorpusNamespace = "http://www.dspin.de/data/textcorpus"

-- | Default value for the base of the numeric part of the IDs used in
-- a TCF file: 10.
defaultTcfIdBase :: Int
defaultTcfIdBase = 10

-- | Default value for the delimiter between the prefix part and the
-- numeric part of IDs used in a TCF file: \'_\'.
defaultTcfIdPrefixDelimiter :: Char
defaultTcfIdPrefixDelimiter = '_'

-- | Returns the maybe qualified named of the text root defined in the
-- config. If there where multiple text roots defined in the config
-- file, the qualified name of the first definition and only this one
-- is returned.
getTextRoot :: [Config] -> Maybe QName
getTextRoot [] = Nothing
getTextRoot ((TextRoot qn):_) = Just qn
getTextRoot (_:xs) = getTextRoot xs

-- | Returns a list of hyphen characters defined in the
-- config. Cf. 'Hyphen'.
getHyphens :: [Config] -> [Char]
getHyphens [] = []
getHyphens ((Hyphen c):xs) = c : getHyphens xs
getHyphens (_:xs) = getHyphens xs

-- | Returns a list of the qualified names of element nodes, that
-- produce linebreak control characters for the
-- tokenizer. Cf. 'LineBreak'.
getLineBreaks :: [Config] -> [QName]
getLineBreaks [] = []
getLineBreaks ((LineBreak qn):xs) = qn : getLineBreaks xs
getLineBreaks (_:xs) = getLineBreaks xs

-- | Returns a list of qualified names of element nodes, that are
-- dropped by the parser. Cf. 'DroppedTree'.
getDroppedTrees :: [Config] -> [QName]
getDroppedTrees [] = []
getDroppedTrees ((DroppedTree qn):xs) = qn : getDroppedTrees xs
getDroppedTrees (_:xs) = getDroppedTrees xs

-- | Get the namespace of the text corpus element of a TCF file. If
-- none is given in the config file, this defaults to
-- 'defaultTcfTextCorpusNamespace'.
getTcfTextCorpusNamespace :: [Config] -> String
getTcfTextCorpusNamespace [] = defaultTcfTextCorpusNamespace
getTcfTextCorpusNamespace ((TcfTextCorpusNamespace ns):_) = ns
getTcfTextCorpusNamespace (_:xs) = getTcfTextCorpusNamespace xs

-- | Get the base of the IDs used in a TCF file. Defaults to
-- 'defaultTcfIdBase'.
getTcfIdBase :: [Config] -> Int
getTcfIdBase [] = defaultTcfIdBase
getTcfIdBase ((TcfIdBase b):_) = b
getTcfIdBase (_:xs) = getTcfIdBase xs

-- | Get the delimiter, that separates the prefix from a numeric part
-- of an ID used in a TCF file. Defaults to
-- 'defaultTcfIdPrefixDelimiter'.
getTcfIdPrefixDelimiter :: [Config] -> Char
getTcfIdPrefixDelimiter [] = defaultTcfIdPrefixDelimiter
getTcfIdPrefixDelimiter ((TcfIdPrefixDelimiter d):_) = d
getTcfIdPrefixDelimiter (_:xs) = getTcfIdPrefixDelimiter xs

-- | An arrow for parsing the config file. Cf. implementation of
-- 'runConfigParser' for usage.
parseConfig :: IOSArrow XmlTree Config
parseConfig =
  (hasName "textRoot" >>> textRoot)
  <+>
  (hasName "droppedTree" >>> getChildren >>>
  hasName "simpleElement" >>> droppedTreeSimple)
  <+>
  (hasName "tokenizerControl" >>> getChildren >>>
  hasName "linebreak" >>> linebreak)
  <+>
  (hasName "specialCharacter" >>> getChildren >>>
  hasName "hyphen" >>> hyphen)
  <+>
  (hasName "tcf" >>> getChildren >>>
  hasName "tcfNamespace" >>> tcfTextCorpusNamespace)
  <+>
  (hasName "tcf" >>> getChildren >>>
  hasName "tcfIdBase" >>> tcfIdBase)
  <+>
  (hasName "tcf" >>> getChildren >>>
  hasName "tcfIdPrefixDelimiter" >>> tcfIdPrefixDelimiter)

textRoot :: IOSArrow XmlTree Config
textRoot =
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (TextRoot . (uncurry mkNsName))

droppedTreeSimple :: IOSArrow XmlTree Config
droppedTreeSimple =
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (DroppedTree . (uncurry mkNsName))

linebreak :: IOSArrow XmlTree Config
linebreak =
  getAttrValue0 "name" &&&
  getAttrValue0 "namespace" >>>
  arr (LineBreak . (uncurry mkNsName))

hyphen :: IOSArrow XmlTree Config
hyphen =
  getAttrValue0 "char" >>>
  arr (Hyphen . head)

tcfTextCorpusNamespace :: IOSArrow XmlTree Config
tcfTextCorpusNamespace =
  getAttrValue "namespace" >>>
  arr (TcfTextCorpusNamespace . defaultOnNull defaultTcfTextCorpusNamespace)

tcfIdBase :: IOSArrow XmlTree Config
tcfIdBase =
  getAttrValue "base" >>>
  arr (TcfIdBase . fromMaybe defaultTcfIdBase . fmap fst . C.readInt . C.pack) 

tcfIdPrefixDelimiter :: IOSArrow XmlTree Config
tcfIdPrefixDelimiter =
  getAttrValue "delimiter" >>>
  arr (TcfIdPrefixDelimiter . head . defaultOnNull (defaultTcfIdPrefixDelimiter:[]))


defaultOnNull :: [a] -> [a] -> [a]
defaultOnNull deflt [] = deflt
defaultOnNull _ (x:xs) = x:xs


-- | Returns the config defined in a XML config file.
runConfigParser :: FilePath -> IO [Config]
runConfigParser fname = do
  results <- runX (readDocument [withValidate no] fname >>>
                   propagateNamespaces //>
                   hasName "config" >>>
                   multi parseConfig)
  return results

