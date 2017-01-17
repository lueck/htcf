module Text.XML.TCF.Parser.TcfLayerParser
  ( mkTcfElement
  ) where

import Text.XML.HXT.Core

import Text.XML.TCF.Parser.TcfElement
import Text.XML.TCF.Parser.TextLayerParser
import Text.XML.TCF.Parser.StructureLayerParser


mkTcfElement :: IOSArrow XmlTree TcfElement
mkTcfElement =
  (isElem >>> mkTcfStructure) <+>
  (isText >>> mkTcfText)
