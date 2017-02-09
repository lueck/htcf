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
  mkTcfStructure <+>
  mkTcfText <+>
  mkTcfLineBreak cfg

mkTcfElementButStructure :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfElementButStructure cfg =
  mkTcfText <+>
  mkTcfLineBreak cfg

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
  getXmlPosition >>>
  arr (\(t, (tOffset, xPos)) -> TcfText t tOffset (fst xPos))

mkTcfTextWithoutSrcPos :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfTextWithoutSrcPos =
  getText &&&
  arr (const 0) &&&         -- text offset
  arr (const Nothing) >>>   -- without source offset
  arr3 TcfText

-- | An arrow for parsing tags into the structure layer. It parses
-- structure elements regardless if they have a source position or
-- not.
mkTcfStructure :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfStructure =
  isElem >>>
  mkTcfStructureWithSrcPos `orElse` mkTcfStructureWithoutSrcPos

mkTcfStructureWithSrcPos :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfStructureWithSrcPos =
  getQName &&&
  arr (const 0) &&&
  arr (const 0) &&&
  getXmlPosition >>>
  arr (\(qN, (tStart, (tEnd, xPos)))
       -> TcfStructure qN tStart tEnd (fst xPos) (snd xPos))

mkTcfStructureWithoutSrcPos :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfStructureWithoutSrcPos =
  getQName &&&
  arr (const 0) &&&
  arr (const 0) &&&
  arr (const (Nothing, Nothing)) >>>
  arr (\(qN, (tStart, (tEnd, xPos)))
       -> TcfStructure qN tStart tEnd (fst xPos) (snd xPos))

-- | An arrow for parsing line breaks and gerating a signal for the
-- tokenizer.
mkTcfLineBreak :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfLineBreak cfg =
  isElem >>>
  (qNameIn $ getLineBreaks cfg) >>>
  arr (const TcfLineBreak)
