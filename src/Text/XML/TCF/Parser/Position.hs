module Text.XML.TCF.Parser.Position
  where

import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN

type TextPosition = Int

type XmlPosition = Int

getXmlPosition :: XN.NTree XNode -> (XmlPosition, XmlPosition)
getXmlPosition _ = (0, 0)

