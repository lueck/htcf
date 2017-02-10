module HTCF.Position
  where

import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN

import HTCF.Utils

type TextPosition = Int

type XmlPosition = Int

shiftTextPosition :: Int -> TextPosition -> TextPosition
shiftTextPosition i pos = pos + i

shiftXmlPosition :: Maybe XmlPosition -> Int -> Maybe XmlPosition
shiftXmlPosition pos i = fmap (+i) pos



-- * Storing positions a the XmlTree.

posPiName :: String
posPiName = "position"

posNamespace :: String
posNamespace = "http://github.com/lueck/HTCF/"

posStartLn :: String
posStartLn = "startLine"

posStartCol :: String
posStartCol = "startColumn"

posEndLn :: String
posEndLn = "endLine"

posEndCol :: String
posEndCol = "endColumn"

mkPositionAttrs :: SourcePos -> SourcePos -> XmlTrees
mkPositionAttrs start end = [sL, sC, eL, eC]
 where
   sL = mkPosAttr posStartLn $ sourceLine start
   sC = mkPosAttr posStartCol $ sourceColumn start
   eL = mkPosAttr posEndLn $ sourceLine end
   eC = mkPosAttr posEndCol $ sourceColumn end
   mkPosAttr n v = XN.mkAttr' (mkNsName n posNamespace) [XN.mkText' $ show $ v]
  
-- | Save start and end position in a processing instruction.
mkPositionNode :: SourcePos -> SourcePos -> XmlTree
mkPositionNode start end =
  XN.mkPi qName $ mkPositionAttrs start end
  where
    qName = mkNsName posPiName posNamespace
    

-- * Arrows for retrieving the position

-- | Arrow for retrieving the position of an arbitrary XNode. Returns
-- a tuple of Just start and end xml positions. If no position if
-- found, a tuple of Nothing is returned. The list of line offsets is
-- expected to be in user state.
getXmlPosition :: IOSLA (XIOState [Int]) XmlTree ((Maybe XmlPosition), (Maybe XmlPosition))
getXmlPosition =
  -- get PI with position attributes (XText, XCharRef)
  (getChildren >>> isPi >>> hasQName (mkNsName posPiName posNamespace))
  -- or elese get the position from the attributes of node itself
  -- (XNode, XPi)
  `orElse` this >>>
  getPosAttrs &&& getUserState >>> -- pass attributes and user state
  arr2 getXmlPos
  where
    getXmlPos _ [] = (Nothing, Nothing) -- redundant, because length tested
    getXmlPos ((Just sL), (Just sC), (Just eL), (Just eC)) lineOffsets
      | (eL-1) <= length lineOffsets = ( (Just ((lineOffsets !! (sL-1)) + sC))
                                       , (Just ((lineOffsets !! (eL-1)) + eC)))
      | otherwise = (Nothing, Nothing)
    getXmlPos _ _ = (Nothing, Nothing)

getPosAttrs :: (ArrowXml a) => a XmlTree ((Maybe Int), (Maybe Int), (Maybe Int), (Maybe Int))
getPosAttrs =
  getQAttrValue (mkNsName posStartLn posNamespace) &&&
  getQAttrValue (mkNsName posStartCol posNamespace) &&&
  getQAttrValue (mkNsName posEndLn posNamespace) &&&
  getQAttrValue (mkNsName posEndCol posNamespace) >>>
  arr4 mkXmlPos
  where
    mkXmlPos :: String -> String -> String -> String -> ((Maybe Int), (Maybe Int), (Maybe Int), (Maybe Int))
    mkXmlPos sL sC eL eC
      = ( (readIntMaybe $ Just sL)
        , (readIntMaybe $ Just sC)
        , (readIntMaybe $ Just eL)
        , (readIntMaybe $ Just eC))
