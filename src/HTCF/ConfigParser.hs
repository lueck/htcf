-- | A config file will be parsed by 'runConfigParser' into a list of
-- @['Config']@. Use 'getTextRoot', 'getHyphens', etc. to get an
-- aspect of the configuration from this list.

module HTCF.ConfigParser
  ( Config (..)
  , UnprefixMethod (..)
  , runConfigParser
  , getTextRoot
  , getHyphens
  , getLineBreaks
  , getDroppedTrees
  , getTcfTextCorpusNamespace
  , getTcfIdBase
  , setTcfIdBase
  , getTcfIdPrefixDelimiter
  , getTcfIdPrefixLength
  , setTcfIdPrefixLength
  , getTcfIdUnprefixMethod
  , getAbbreviations
  , addAbbreviations
  , getMonths
  , abbrev1CharTokenP
  , singleDigitOrdinalP
  , getNoBreaks
  , defaultTcfTextCorpusNamespace
  , defaultTcfIdBase
  , defaultTcfIdPrefixDelimiter
  , defaultTcfIdPrefixLength
  , defaultTcfIdUnprefixMethod
  , defaultAbbrev1CharToken
  , defaultSingleDigitOrdinal
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
  , pcTcfIdPrefixDelimiter
  , pcTcfIdPrefixLength
  ) where

import Text.XML.HXT.Core
import qualified Data.ByteString.Char8 as C
import Data.Maybe

-- * Types

data UnprefixMethod = Delimiter | Length deriving (Show, Eq)

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
                                        -- the number part in a ID
                                        -- used in a TCF file
  | TcfIdPrefixLength Int               -- ^ The length of the prefix
                                        -- in an ID used in a TCF
                                        -- file.
  | TcfIdUnprefixMethod UnprefixMethod  -- ^ Howto unprefix an ID used
                                        -- in a TCF file.
  | Abbreviation String                 -- ^ An abbreviation (string
                                        -- without ending dot)
  | Month String                        -- ^ A month. Months are
                                        -- needed for abbreviation and
                                        -- tokenization of days.
  | Abbrev1CharToken Bool               -- ^ Holds a boolean value,
                                        -- whether tokens with a
                                        -- length of one word followed
                                        -- by a punct are treated as a
                                        -- abbreviation.
  | SingleDigitOrdinal Bool             -- ^ Holds a boolean value,
                                        -- whether single digits
                                        -- followed by a punct are
                                        -- treated as an ordinal.
  | NoBreak Char                        -- ^ A character which by its
                                        -- category results in a
                                        -- break, i.e. punctuation or
                                        -- space, but which is not
                                        -- supposed to do so.
  deriving (Show, Eq)

-- * Default configuration values.

-- | Default qualified name of the element node containing the text
-- that is fed to the tokenizer etc. Defaults to TEI's \"text\"
-- element.
defaultTextRoot :: QName
defaultTextRoot = mkNsName "text" "http://www.tei-c.org/ns/1.0"

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

-- | Default length of the prefix in an ID used in a TCF file: 1.
defaultTcfIdPrefixLength :: Int
defaultTcfIdPrefixLength = 2

-- | Default method how to strip the prefix in an ID used in a TCF
-- file: 'Length'.
defaultTcfIdUnprefixMethod :: UnprefixMethod
defaultTcfIdUnprefixMethod = Length

-- | By default, tokens of the length of 1 character are not treated
-- as abbrevs.
defaultAbbrev1CharToken :: Bool
defaultAbbrev1CharToken = False

-- | By default, numbers of the lenght of 1 character are not treated
-- as ordinals.
defaultSingleDigitOrdinal :: Bool
defaultSingleDigitOrdinal = False

-- * Getting aspects of the configuration.

-- | Returns the maybe qualified named of the text root defined in the
-- config. If there where multiple text roots defined in the config
-- file, the qualified name of the first definition and only this one
-- is returned.
getTextRoot :: [Config] -> QName
getTextRoot [] = defaultTextRoot
getTextRoot ((TextRoot qn):_) = qn
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

-- | Get the length of the prefix preceding the numeric part in an ID
-- used in a TCF file. Defaults to 'defaultTcfPrefixLength'.
getTcfIdPrefixLength :: [Config] -> Int
getTcfIdPrefixLength [] = defaultTcfIdPrefixLength
getTcfIdPrefixLength ((TcfIdPrefixLength l):_) = l
getTcfIdPrefixLength (_:xs) = getTcfIdPrefixLength xs

-- | Get the method howto strip the prefix from an ID used in a TCF
-- file.
getTcfIdUnprefixMethod :: [Config] -> UnprefixMethod
getTcfIdUnprefixMethod [] = defaultTcfIdUnprefixMethod
getTcfIdUnprefixMethod ((TcfIdUnprefixMethod Length):_) = Length
getTcfIdUnprefixMethod ((TcfIdUnprefixMethod Delimiter):_) = Delimiter
getTcfIdUnprefixMethod (_:xs) = getTcfIdUnprefixMethod xs

-- | Get the list of abbreviation strings from config.
getAbbreviations :: [Config] -> [String]
getAbbreviations [] = []
getAbbreviations ((Abbreviation abbr):xs) = abbr:getAbbreviations xs
getAbbreviations (_:xs) = getAbbreviations xs

-- | Get characters from the config which would result in a break
-- between two tokens, but which are not supposed to.
getNoBreaks :: [Config] -> [Char]
getNoBreaks [] = []
getNoBreaks ((NoBreak c):xs) = c : getNoBreaks xs
getNoBreaks (_:xs) = getNoBreaks xs

-- | Get the list of month names form config.
getMonths :: [Config] -> [String]
getMonths [] = []
getMonths ((Month m):xs) = m:getMonths xs
getMonths (_:xs) = getMonths xs

-- | Test if abbreviation of 1 char length tokens is active or not.
abbrev1CharTokenP :: [Config] -> Bool
abbrev1CharTokenP [] = defaultAbbrev1CharToken
abbrev1CharTokenP ((Abbrev1CharToken b):_) = b
abbrev1CharTokenP (_:xs) = abbrev1CharTokenP xs

-- | Test if one digit numbers are read as ordinals.
singleDigitOrdinalP :: [Config] -> Bool
singleDigitOrdinalP [] = defaultSingleDigitOrdinal
singleDigitOrdinalP ((SingleDigitOrdinal b):_) = b
singleDigitOrdinalP (_:xs) = singleDigitOrdinalP xs


-- * Setters for aspects of the configuration.

-- | Set the base of the IDs used in a TCF file.
setTcfIdBase :: Int       -- ^ the base
             -> [Config]  -- ^ the existing config
             -> [Config]  -- ^ the new configuration is returned
setTcfIdBase bs cfg = (TcfIdBase bs) : cfg

-- | Set the length of the prefix preceding the numeric part in an ID
-- used in a Tcf file. This overrides the value parsed from the config
-- file.
setTcfIdPrefixLength :: Int       -- ^ the length of the ID prefix
                     -> [Config]  -- ^ the existing configuration
                     -> [Config]  -- ^ the new configuration is returned
setTcfIdPrefixLength l cfg = (TcfIdPrefixLength l) : cfg                     

-- | Add a list of strings to the known abbreviations
-- (cf. 'Abbreviation').
addAbbreviations :: [String] -- ^ the list of abbreviations
                 -> [Config] -- ^ the existing config
                 -> [Config] -- ^ returned config with new abbrevs
addAbbreviations abbrevs cfg = cfg ++ (map (Abbreviation) abbrevs)


-- * Parsing the xml config file.

-- | Returns the config defined in a XML config file.
runConfigParser :: FilePath -> IO [Config]
runConfigParser fname = do
  results <- runX (readDocument [withValidate no] fname >>>
                   propagateNamespaces //>
                   hasName "config" >>>
                   multi parseConfig)
  return results

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
  pcTcfTextCorpusNamespace <+>
  pcTcfIdBase <+>
  pcTcfIdPrefixDelimiter <+>
  pcTcfIdPrefixLength

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

pcTcfTextCorpusNamespace :: IOSArrow XmlTree Config
pcTcfTextCorpusNamespace =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfNamespace" >>>
  getAttrValue "namespace" >>>
  arr (TcfTextCorpusNamespace . defaultOnNull defaultTcfTextCorpusNamespace)

pcTcfIdBase :: IOSArrow XmlTree Config
pcTcfIdBase =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfIdBase" >>>   getAttrValue "base" >>>
  arr (TcfIdBase . fromMaybe defaultTcfIdBase . fmap fst . C.readInt . C.pack) 

pcTcfIdPrefixDelimiter :: IOSArrow XmlTree Config
pcTcfIdPrefixDelimiter =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfIdPrefix" >>>
  getAttrValue "delimiter" >>>
  arr (TcfIdPrefixDelimiter . head . defaultOnNull (defaultTcfIdPrefixDelimiter:[]))

pcTcfIdPrefixLength :: IOSArrow XmlTree Config
pcTcfIdPrefixLength =
  hasName "tcf" >>> getChildren >>>
  hasName "tcfIdPrefix" >>>
  getAttrValue "length" >>>
  arr (TcfIdPrefixLength . fromMaybe defaultTcfIdPrefixLength . fmap fst . C.readInt . C.pack)

defaultOnNull :: [a] -> [a] -> [a]
defaultOnNull deflt [] = deflt
defaultOnNull _ (x:xs) = x:xs
