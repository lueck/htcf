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
mkTcfElement :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfElement cfg =
  (isElem >>> mkTcfStructure) <+>
  mkTcfText <+>
  (isElem >>> mkTcfLineBreak cfg)

mkTcfElementButStructure :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfElementButStructure cfg =
  mkTcfText <+>
  (isElem >>> mkTcfLineBreak cfg)

-- | An arrow for parsing text nodes into the text layer.  It parses
-- text nodes regardless if they have a source position or not.
mkTcfText :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfText =
  isText >>>
  mkTcfTextWithSrcPos `orElse` mkTcfTextWithoutSrcPos

mkTcfTextWithSrcPos :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfTextWithSrcPos =
  getText &&&
  arr (const 0) &&&    -- text offset
  getTextXmlPosition >>>
  arr3 TcfText

mkTcfTextWithoutSrcPos :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfTextWithoutSrcPos =
  getText &&&
  arr (const 0) &&&         -- text offset
  arr (const Nothing) >>>   -- without source offset
  arr3 TcfText

-- | An arrow for parsing tags into the structure layer
--
-- Usage: @isElem >>> mkTcfStructure@
mkTcfStructure :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfStructure =
  getQName &&&
  arr (const 0) &&&
  arr (const 0) &&&
  arr getXmlPosition >>>
  arr (\(qN, (tStart, (tEnd, xPos)))
       -> TcfStructure qN tStart tEnd (fst xPos) (snd xPos))

mkTcfLineBreak :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfLineBreak cfg =
  (qNameIn $ getLineBreaks cfg) >>>
  arr (const TcfLineBreak)
