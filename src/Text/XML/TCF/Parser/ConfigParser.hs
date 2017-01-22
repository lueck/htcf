-- | A config file will be parsed by 'runConfigParser' into a list of
-- @['Config']@. Use 'getTextRoot', 'getHyphens', etc. to get an
-- aspect of the configuration from this list.

module Text.XML.TCF.Parser.ConfigParser
  ( Config (..)
  , runConfigParser
  , getTextRoot
  , getHyphens
  , getLineBreaks
  , getDroppedTrees
  ) where

import Text.XML.HXT.Core

data Config =
  TextRoot QName                        -- ^ qname of parent node for text
  | DroppedTree                         -- ^ xml subtree to be dropped by the parser
  { qName :: QName }                    -- ^ qname of element node
  | LineBreak QName                     -- ^ qname of element
                                        -- resulting in a control sign
                                        -- for the tokenizer
  | Hyphen Char                         -- ^ hyphen character for tokenizer
  deriving (Show, Eq)

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

-- | An arrow for parsing the config file.
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

-- | Returns the config from a file. 
runConfigParser :: FilePath -> IO [Config]
runConfigParser fname = do
  results <- runX (readDocument [withValidate no] fname >>>
                   propagateNamespaces //>
                   hasName "config" >>>
                   multi parseConfig)
  return results

