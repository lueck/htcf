module HTCF.TcfParser
  ( mkTcfElement
  , mkTcfElementButStructure
  , mkTcfText
  , mkTcfStructure
  , mkTcfLineBreak
  ) where

import Text.XML.HXT.Core

import HTCF.TcfParserTypeDefs
import HTCF.Position
import HTCF.ConfigParser
import HTCF.ArrowXml

-- | @mkTcfElement@ can be used to generate a (deterministic) list of
-- 'TcfElement's, which can then be feed to the tokenizer or a
-- serializer for the text layer or structure layer. It simply
-- combines the arrows for the different layers and additional
-- tokenizer information.
--
-- Usage: @multi mkTcfElement@
mkTcfElement :: [Config] -> IOSLA (XIOState u) XmlTree TcfElement
mkTcfElement cfg =
  (isElem >>> mkTcfStructure) <+>
  (isText >>> mkTcfText) <+>
  (isElem >>> mkTcfLineBreak cfg)

mkTcfElementButStructure :: [Config] -> IOSLA (XIOState u) XmlTree TcfElement
mkTcfElementButStructure cfg =
  (isText >>> mkTcfText) <+>
  (isElem >>> mkTcfLineBreak cfg)

-- | An arrow for parsing text nodes into the text layer
--
-- Usage: @isText >>> mkTcfText@
mkTcfText :: IOSLA (XIOState u) XmlTree TcfElement
mkTcfText =
  getText &&&
  arr (const 0) &&&
  getTextXmlPosition >>>
  --arr (fst . getXmlPosition) >>>
  arr3 TcfText

-- | An arrow for parsing tags into the structure layer
--
-- Usage: @isElem >>> mkTcfStructure@
mkTcfStructure :: (ArrowXml a) => a XmlTree TcfElement
mkTcfStructure =
  getQName &&&
  arr (const 0) &&&
  arr (const 0) &&&
  arr getXmlPosition >>>
  arr (\(qN, (tStart, (tEnd, xPos)))
       -> TcfStructure qN tStart tEnd (fst xPos) (snd xPos))

mkTcfLineBreak :: (ArrowXml a) => [Config] -> a XmlTree TcfElement
mkTcfLineBreak cfg =
  (qNameIn $ getLineBreaks cfg) >>>
  arr (const TcfLineBreak)
