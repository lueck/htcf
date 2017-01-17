module Text.XML.TCF.Parser.TcfElement
  ( TcfElement (..)
  , tcfTextLen
  , dupWithNewTextPos
  , propagateOffsets
  )
  where

import Text.XML.HXT.Core

import Text.XML.TCF.Parser.Position
       
data TcfElement = TcfText
  { text :: String
  , textOffset :: TextPosition
  , xmlOffset :: XmlPosition
  }
  | TcfStructure
  { qName :: QName
  , textStart :: TextPosition
  , textEnd :: TextPosition
  , xmlStart :: XmlPosition
  , xmlEnd :: XmlPosition
  } deriving (Show)

tcfTextLen :: TcfElement -> Int
tcfTextLen (TcfText t _ _) = length t
tcfTextLen _ = 0

dupWithNewTextPos :: TcfElement -> TextPosition -> TcfElement
dupWithNewTextPos (TcfText tx _ xOffset) i =
  TcfText tx i xOffset
dupWithNewTextPos (TcfStructure qN _ _ xStart xEnd) i =
  TcfStructure qN i 0 xStart xEnd

propagateOffsets :: [TcfElement] -> [TcfElement]
propagateOffsets xs = propagateOffsets' 0 xs
  where
    propagateOffsets' :: TextPosition -> [TcfElement] -> [TcfElement]
    propagateOffsets' i (x:[]) = [dup x i]
    propagateOffsets' i (x:xs) = dup x i : propagateOffsets' (i+(tcfTextLen x)+(incValue x)) xs
    dup = dupWithNewTextPos
    -- Increment by 1 for TcfText, but 0 for others
    -- FIXME: Do we really need this incrementation? Verification needed!
    incValue (TcfText _ _ _) = 1
    incValue _ = 0
