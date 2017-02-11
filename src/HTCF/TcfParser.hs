module HTCF.TcfParser
  ( mkTcfElement
  , mkTcfElementButStructure
  , mkTcfText
  , mkTcfTextFromCharRef
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
  mkTcfTextFromCharRef <+>
  mkTcfLineBreak cfg

-- | Like 'mkTcfElement', but without structure elements. Use this
-- instead of @mkTcfElement@ if no structure layer is to be produced.
mkTcfElementButStructure :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfElementButStructure cfg =
  mkTcfText <+>
  mkTcfTextFromCharRef <+>
  mkTcfLineBreak cfg

-- | An arrow for parsing text nodes into the text layer.
mkTcfText :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfText =
  getText &&&
  arr (const 0) &&&    -- text offset
  getXmlPosition >>>
  arr (\(t, (tOffset, xPos)) -> TcfText t tOffset (mkCharPositions t xPos))
  where
    mkCharPositions t ((Just start), (Just end))
      | length t == end - start = [((Just i), (Just i)) | i <- [start .. end]]
    mkCharPositions t (_, _)
      = replicate (length t) (Nothing, Nothing)

-- | An arrow for parsing char refs into the text layer.
mkTcfTextFromCharRef :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfTextFromCharRef =
  isCharRef >>>
  getCharRef &&&
  arr (const 0) &&&
  getXmlPosition >>>
  arr (\(i, (tOffset, xPos)) -> TcfText [toEnum i] tOffset [xPos])

-- | An arrow for parsing tags into the structure layer.
mkTcfStructure :: IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfStructure =
  getQName &&&
  arr (const 0) &&&
  arr (const 0) &&&
  getXmlPosition >>>
  arr (\(qN, (tStart, (tEnd, xPos)))
       -> TcfStructure qN tStart tEnd (fst xPos) (snd xPos))

-- | An arrow for parsing line breaks and gerating a signal for the
-- tokenizer.
mkTcfLineBreak :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfLineBreak cfg =
  isElem >>>
  (qNameIn $ getLineBreaks cfg) >>>
  arr (const TcfLineBreak)
