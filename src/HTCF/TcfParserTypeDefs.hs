module HTCF.TcfParserTypeDefs
  ( TcfElement (..)
  , isTcfText
  , isTcfStructure
  , isTcfLineBreak
  , tcfTextLen
  , dupWithNewTextPos
  , propagateOffsets
  , getTcfText
  , getTcfQName
  , getTextOffset
  , getTextLength
  , getSrcCharOffsets
  , getSrcStartPos
  , getSrcEndPos
  , serialize
  )
  where

import Text.XML.HXT.Core

import HTCF.Position
       
data TcfElement =
  TcfText                          -- ^ constituent of text layer
  { text :: String                 -- ^ the text
  , textOffset :: TextPosition     -- ^ offset in text layer
  , charPos :: [((Maybe XmlPosition), (Maybe XmlPosition))]
  }
  | TcfStructure                   -- ^ constituent of structure layer
  { qName :: QName                 -- ^ qualified name of tag
  , textStart :: TextPosition      -- ^ start position in text layer
  , textLength :: Int              -- ^ length in text layer
  , srcStart :: Maybe XmlPosition        -- ^ start position in source
  , srcEnd :: Maybe XmlPosition          -- ^ end position in end
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
dupWithNewTextPos (TcfText tx _ cPos) i =
  TcfText tx i cPos
dupWithNewTextPos (TcfStructure qN _ tLength xStart xEnd) i =
  TcfStructure qN i tLength xStart xEnd
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

getTcfQName :: TcfElement -> Maybe QName
getTcfQName (TcfStructure qn _ _ _ _) = Just qn
getTcfQName _ = Nothing

getTextOffset :: TcfElement -> TextPosition
getTextOffset (TcfText _ p _) = p
getTextOffset (TcfStructure _ p _ _ _) = p
getTextOffset _ = 0

getTextLength :: TcfElement -> Int
getTextLength (TcfText t _ _) = length t
getTextLength (TcfStructure _ _ l _ _) = l
getTextLength _ = 0

getSrcCharOffsets :: TcfElement -> [((Maybe XmlPosition), (Maybe XmlPosition))]
getSrcCharOffsets (TcfText _ _ charOffsets) = charOffsets
getSrcCharOffsets _ = [(Nothing, Nothing)]

getSrcStartPos :: TcfElement -> Maybe XmlPosition
getSrcStartPos (TcfText _ _ charOffsets) = fst $ head charOffsets
getSrcStartPos (TcfStructure _ _ _ s _) = s
getSrcStartPos _ = Nothing

getSrcEndPos :: TcfElement -> Maybe XmlPosition
getSrcEndPos (TcfText _ _ charOffsets) = snd $ last charOffsets
getSrcEndPos (TcfStructure _ _ _ _ e) = e
getSrcEndPos _ = Nothing

serialize :: TcfElement -> String
serialize (TcfText t _ _) = t
serialize (TcfStructure qN _ _ _ _) = "{{ " ++ show qN ++ " }}"
serialize (TcfLineBreak) = "$LineBreak$"
