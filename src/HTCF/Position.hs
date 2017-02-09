module HTCF.Position
  where

import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN

import HTCF.Utils

type TextPosition = Int

type XmlPosition = Int

getXmlPosition :: XN.NTree XNode -> (XmlPosition, XmlPosition)
getXmlPosition _ = (0, 0)

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

--getTextXmlPosition' :: (ArrowXml a) => a XmlTree (Maybe XmlPosition)
getTextXmlPosition :: IOSLA (XIOState u) XmlTree (Maybe XmlPosition)
getTextXmlPosition =
  getChildren >>> isPi >>> hasQName (mkNsName posPiName posNamespace) >>> getPosAttrs
  &&& getUserState
  >>> arr2 getXmlPos
  where
    -- FIXME
    getXmlPos ((Just sL), (Just sC), _, _) _ = Just (sL*80 + sC) 
    getXmlPos _ _ = Nothing
  
getPosAttrs :: (ArrowXml a) => a XmlTree ((Maybe XmlPosition), (Maybe XmlPosition), (Maybe XmlPosition), (Maybe XmlPosition))
getPosAttrs =
  getQAttrValue (mkNsName posStartLn posNamespace) &&&
  getQAttrValue (mkNsName posStartCol posNamespace) &&&
  getQAttrValue (mkNsName posEndLn posNamespace) &&&
  getQAttrValue (mkNsName posEndCol posNamespace) >>>
  arr4 mkXmlPos
  where
    mkXmlPos :: String -> String -> String -> String -> ((Maybe XmlPosition), (Maybe XmlPosition), (Maybe XmlPosition), (Maybe XmlPosition))
    mkXmlPos sL sC eL eC
      = ( (readIntMaybe $ Just sL)
        , (readIntMaybe $ Just sC)
        , (readIntMaybe $ Just eL)
        , (readIntMaybe $ Just eC))
