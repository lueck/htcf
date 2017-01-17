module Text.XML.TCF.Parser.ConfigParser
  ( stripped
  , runConfigParser
  ) where

import Text.XML.HXT.Core

stripped :: FilePath -> IOSArrow XmlTree QName
stripped fname =
  readDocument [withValidate no] fname >>>
  getChildren >>>
  isElem >>> hasName "config" >>>
  getChildren >>>
  isElem >>> hasName "strippedText" >>>
  getChildren >>>
  isElem >>> hasName "simpleElement" >>> strippedQName

strippedQName :: IOSArrow XmlTree QName
strippedQName =
  getAttrValue0 "namespace" &&&
  getAttrValue0 "name" >>>
  arr (uncurry mkNsName)

runConfigParser :: FilePath -> IO [QName]
runConfigParser fname = do
  results <- runX (stripped fname)
  return results

