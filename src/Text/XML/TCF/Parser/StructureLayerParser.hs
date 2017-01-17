module Text.XML.TCF.Parser.StructureLayerParser
  ( mkTcfStructure
  ) where

import Text.XML.HXT.Core

import Text.XML.TCF.Parser.Position
import Text.XML.TCF.Parser.TcfElement

-- Usage: isElem >>> mkTcfStructure
mkTcfStructure :: IOSArrow XmlTree TcfElement
mkTcfStructure =
  getQName &&&
  arr (const 0) &&&
  arr (const 0) &&&
  arr getXmlPosition >>>
  arr (\(qN, (tStart, (tEnd, xPos)))
       -> PositionedStructure qN tStart tEnd (fst xPos) (snd xPos))


-- something to play with interactively
play :: FilePath -> IO ()
play fName = do
  results <- runX (readDocument [withValidate no] fName >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   multi isText >>> mkTcfStructure)
  print $ propagateOffsets results
