module HTCF.Position
  where

import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN

type TextPosition = Int

type XmlPosition = Int

getXmlPosition :: XN.NTree XNode -> (XmlPosition, XmlPosition)
getXmlPosition _ = (0, 0)

shiftTextPosition :: Int -> TextPosition -> TextPosition
shiftTextPosition i pos = pos + i

shiftXmlPosition :: Int -> XmlPosition -> XmlPosition
shiftXmlPosition i pos = pos + i
