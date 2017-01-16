module Text.XML.TCF.Parser.TextLayerParser
  ( mkTcfText
  ) where

import Text.XML.HXT.Core

import Text.XML.TCF.Parser.Position
import Text.XML.TCF.Parser.TcfElement

mkTcfText :: IOSArrow XmlTree TcfElement
mkTcfText =
  getText &&&
  arr (const 0) &&&
  arr (fst . getXmlPosition) >>>
  arr3 PositionedText


-- something to play with interactively
play :: FilePath -> IO ()
play fName = do
  results <- runX (readDocument [withValidate no] fName >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   multi isText >>> mkTcfText)
  print $ propagateOffsets results
