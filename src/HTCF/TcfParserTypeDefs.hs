module HTCF.TcfParserTypeDefs
  ( TcfElement (..)
  , isTcfText
  , isTcfStructure
  , isTcfLineBreak
  , tcfTextLen
  , dupWithNewTextPos
  , propagateOffsets
  , getTcfText
  , serialize
  )
  where

import Text.XML.HXT.Core

import HTCF.Position
       
data TcfElement =
  TcfText                          -- ^ constituent of text layer
  { text :: String                 -- ^ the text
  , textOffset :: TextPosition     -- ^ offset in text layer
  , srcOffset :: Maybe XmlPosition       -- ^ offset in source
  }
  | TcfStructure                   -- ^ constituent of structure layer
  { qName :: QName                 -- ^ qualified name of tag
  , textStart :: TextPosition      -- ^ start position in text layer
  , textEnd :: TextPosition        -- ^ end position in text layer
  , srcStart :: XmlPosition        -- ^ start position in source
  , srcEnd :: XmlPosition          -- ^ end position in end
  }
  | TcfLineBreak                   -- ^ control sign for the tokenizer
  deriving (Show)

isTcfText :: TcfElement -> Bool
isTcfText (TcfText _ _ _) = True
isTcfText _ = False

isTcfStructure :: TcfElement -> Bool
isTcfStructure (TcfStructure _ _ _ _ _) = True
isTcfStructure _ = False

isTcfLineBreak :: TcfElement -> Bool
isTcfLineBreak (TcfLineBreak) = True
isTcfLineBreak _ = False

tcfTextLen :: TcfElement -> Int
tcfTextLen (TcfText t _ _) = length t
tcfTextLen _ = 0

dupWithNewTextPos :: TcfElement -> TextPosition -> TcfElement
dupWithNewTextPos (TcfText tx _ xOffset) i =
  TcfText tx i xOffset
dupWithNewTextPos (TcfStructure qN _ _ xStart xEnd) i =
  TcfStructure qN i 0 xStart xEnd
dupWithNewTextPos (TcfLineBreak) _ = TcfLineBreak

propagateOffsets :: [TcfElement] -> [TcfElement]
propagateOffsets xs = propagateOffsets' 0 xs
  where
    propagateOffsets' :: TextPosition -> [TcfElement] -> [TcfElement]
    propagateOffsets' _ [] = []
    propagateOffsets' i (x:[]) = [dup x i]
    propagateOffsets' i (x:xs) = dup x i : propagateOffsets' (i+(tcfTextLen x)) xs
    dup = dupWithNewTextPos

getTcfText :: TcfElement -> String
getTcfText (TcfText t _ _) = t
getTcfText _ = ""

serialize :: TcfElement -> String
serialize (TcfText t _ _) = t
serialize (TcfStructure qN _ _ _ _) = "{{ " ++ show qN ++ " }}"
serialize (TcfLineBreak) = "$LineBreak$"
