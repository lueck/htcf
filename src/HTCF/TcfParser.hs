module HTCF.TcfParser
  ( mkTcfElement
  , mkTcfElementButStructure
  , mkTcfText
  , mkTcfTextFromCharRef
  , mkTcfStructure
  , mkTcfLineBreak
  ) where

import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Data.Maybe

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
      | length t == end - start + 1 = [((Just i), (Just i)) | i <- [start .. end]]
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
  isElem >>>
  getQName &&&
  arr (const 0) &&& -- start position in text layer
  arr (length . collectText) &&& -- length in text layer
  getXmlPosition >>>
  arr (\(qN, (tStart, (tLength, xPos)))
       -> TcfStructure qN tStart tLength (fst xPos) (snd xPos))

-- | Collect all text nodes in the current subtree.
collectText :: XmlTree -> String
collectText (XN.NTree n cs)
  | XN.isElem n = concatMap collectText cs
  | XN.isText n = fromMaybe "" $ XN.getText n
  | XN.isCharRef n = fromMaybe "" $ fmap (\i -> [toEnum i]) $ XN.getCharRef n
  | otherwise = ""

-- | An arrow for parsing line breaks and gerating a signal for the
-- tokenizer.
mkTcfLineBreak :: [Config] -> IOSLA (XIOState [Int]) XmlTree TcfElement
mkTcfLineBreak cfg =
  isElem >>>
  (qNameIn $ getLineBreaks cfg) >>>
  arr (const TcfLineBreak)
