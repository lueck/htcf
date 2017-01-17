module Text.XML.TCF.Parser.TcfLayerParser
  ( mkTcfElement
  , mkTcfText
  , mkTcfStructure
  ) where

import Text.XML.HXT.Core

import Text.XML.TCF.Parser.TcfElement
import Text.XML.TCF.Parser.Position

-- | @mkTcfElement@ can be used to generate a (deterministic) list of
-- 'TcfElement's, which can then be feed to the tokenizer or a
-- serializer for the text layer or structure layer. It simply
-- combines the arrows for the different layers and additional
-- tokenizer information.
--
-- Usage: @multi mkTcfElement@
mkTcfElement :: IOSArrow XmlTree TcfElement
mkTcfElement =
  (isElem >>> mkTcfStructure) <+>
  (isText >>> mkTcfText)

-- | An arrow for parsing text nodes into the text layer
--
-- Usage: @isText >>> mkTcfText@
mkTcfText :: IOSArrow XmlTree TcfElement
mkTcfText =
  getText &&&
  arr (const 0) &&&
  arr (fst . getXmlPosition) >>>
  arr3 TcfText

-- | An arrow for parsing tags into the structure layer
--
-- Usage: @isElem >>> mkTcfStructure@
mkTcfStructure :: IOSArrow XmlTree TcfElement
mkTcfStructure =
  getQName &&&
  arr (const 0) &&&
  arr (const 0) &&&
  arr getXmlPosition >>>
  arr (\(qN, (tStart, (tEnd, xPos)))
       -> TcfStructure qN tStart tEnd (fst xPos) (snd xPos))
