module Text.XML.TCF.Parser.TcfElement
  ( TcfElement (..)
  , tcfTextLen
  , dupWithNewTextPos
  )
  where

import Text.XML.HXT.Core

import Text.XML.TCF.Parser.Position
       
data TcfElement = PositionedText
  { text :: String
  , textOffset :: TextPosition
  , xmlOffset :: XmlPosition
  }
  | PositionedStructure
  { qName :: QName
  , textStart :: TextPosition
  , textEnd :: TextPosition
  , xmlStart :: XmlPosition
  , xmlEnd :: XmlPosition
  } deriving (Show)

tcfTextLen :: TcfElement -> Int
tcfTextLen (PositionedText t _ _) = length t
tcfTextLen _ = 0

dupWithNewTextPos :: TcfElement -> TextPosition -> TcfElement
dupWithNewTextPos (PositionedText tx _ xOffset) i =
  PositionedText tx i xOffset
dupWithNewTextPos (PositionedStructure qN _ _ xStart xEnd) i =
  PositionedStructure qN i 0 xStart xEnd

