module HTCF.ArrowXml
  ( nameIn
  , qNameIn
  , stripName
  , stripNames
  , stripQNames
  , getAttrCaseValue
  ) where

import Text.XML.HXT.Core
import Data.Char

nameIn :: (ArrowXml a) => [String] -> a XmlTree XmlTree
nameIn names = (getName >>> isA (flip elem names)) `guards` this
{-# INLINE nameIn #-}

qNameIn :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
qNameIn qNames = (getQName >>> isA (flip elem qNames)) `guards` this
{-# INLINE qNameIn #-}

stripName :: (ArrowXml a) => String -> a XmlTree XmlTree
stripName n = processTopDown (filterA $ neg (hasName n))
{-# INLINE stripName #-}

-- this does not work, we need neg or ifA to pass nodes through
stripName' :: (ArrowXml a) => String -> a XmlTree XmlTree
stripName' n = processTopDown (filterA (getName >>> isA (/= n)) `guards` this)

stripNames :: (ArrowXml a) => [String] -> a XmlTree XmlTree
stripNames names = processTopDown $ neg $ (nameIn names) `guards` this
{-# INLINE stripNames #-}

stripQNames :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
stripQNames qNames = processTopDown $ neg $ (qNameIn qNames) `guards` this

-- | Select the value of an attribute of an element node. The
-- attribute name is matched case insensitive. This always succeeds
-- with an empty string as default value.
getAttrCaseValue :: (ArrowXml a) => String -> a XmlTree String
getAttrCaseValue n =
  xshow (getAttrl >>>
         hasNameWith ((==n') . upper . localPart) >>>
         getChildren)
  where
    n' = upper n
    upper = map toUpper

play :: String -> IO ()
play fname = do
  results <- runX (readDocument [withValidate no] fname >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   stripName "p" >>>
                   getChildren >>>
                   isElem >>>
                   getChildren >>>
                   getText)
  print results

