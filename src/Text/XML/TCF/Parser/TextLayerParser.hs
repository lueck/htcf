module Text.XML.TCF.Parser.TextLayerParser
  ( propagateOffsets
  , mkTcfText
  ) where

import Text.XML.HXT.Core

import Text.XML.TCF.Parser.Position
import Text.XML.TCF.Parser.TcfElement

propagateOffsets :: [TcfElement] -> [TcfElement]
propagateOffsets xs = propagateOffsets' 0 xs
  where
    propagateOffsets' :: TextPosition -> [TcfElement] -> [TcfElement]
    propagateOffsets' i (x:[]) = [dup x i]
    propagateOffsets' i (x:xs) = dup x i : propagateOffsets' (i+(tcfTextLen x)+1) xs
    dup = dupWithNewTextPos

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
