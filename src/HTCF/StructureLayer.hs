{-# LANGUAGE TemplateHaskell #-}

module HTCF.StructureLayer
  where

import Text.XML.HXT.Core
import Data.Maybe
import Control.Lens

import HTCF.Config
import HTCF.TokenLayer
import HTCF.Position
import HTCF.Utils
import qualified HTCF.TcfParserTypeDefs as TP

-- FIXME? Reproduce attributes?

data TextStructure = TextSpan
  { _struct_typE :: Maybe String
  , _struct_namespace :: Maybe String
  , _struct_start :: Maybe Int
  , _struct_end :: Maybe Int
  , _struct_textStart :: Maybe TextPosition
  , _struct_textEnd :: Maybe TextPosition
  , _struct_srcStart :: Maybe XmlPosition
  , _struct_srcEnd :: Maybe XmlPosition
  } deriving (Eq, Show)

makeLenses ''TextStructure

getSrcStartPos :: TextStructure -> Maybe XmlPosition
getSrcStartPos (TextSpan _ _ _ _ _ _ s _) = s
{-# DEPRECATED getSrcStartPos "Use lenses instead!" #-}

getSrcEndPos :: TextStructure -> Maybe XmlPosition
getSrcEndPos (TextSpan _ _ _ _ _ _ _ e) = e
{-# DEPRECATED getSrcEndPos "Use lenses instead!" #-}

getTextStartPos :: TextStructure -> Maybe TextPosition
getTextStartPos (TextSpan _ _ _ _ s _ _ _) = s
{-# DEPRECATED getTextStartPos "Use lenses instead!" #-}

getTextEndPos :: TextStructure -> Maybe TextPosition
getTextEndPos (TextSpan _ _ _ _ _ e _ _) = e
{-# DEPRECATED getTextEndPos "Use lenses instead!" #-}

mkTextSpan :: TP.TcfElement -> Maybe Int -> Maybe Int -> TextStructure
mkTextSpan el startToken endToken =
  (TextSpan
     (fmap localPart $ TP.getTcfQName el)
     (fmap namespaceUri $ TP.getTcfQName el)
     startToken
     endToken
     (Just $ TP.getTextOffset el)
     (Just $ TP.getTextOffset el + TP.getTextLength el)
     (TP.getSrcStartPos el)
     (TP.getSrcEndPos el))

-- | Function for generating the text structure layer.
mkTextSpans :: [Token]           -- ^ the token layer
            -> [TP.TcfElement]   -- ^ list of TcfStructure elements
                                 -- from the TcfParser
            -> [TextStructure]   -- ^ returns the text structure layer
mkTextSpans _ [] = []
mkTextSpans toks@(t:ts) (el:els)
  -- el does not contain any characters (empty element)
  | (TP.getTextLength el) == 0
  = (mkTextSpan el Nothing Nothing) : mkTextSpans toks els
  -- token t left from el: move to next token
  | fromMaybe 0 tEnd < elStart && isJust tEnd
  = mkTextSpans ts (el:els)
  -- structure does not contain any token
  | elEnd < (fromMaybe 0 tStart) && isJust tStart
  = (mkTextSpan el Nothing Nothing) : mkTextSpans toks els
  --- el exceeds one token or a part of a token
  | fromMaybe 0 tStart <= elStart && isJust tStart &&
    fromMaybe 0 tEnd >= elEnd
  = (mkTextSpan el (_token_id t) (_token_id t))
    : mkTextSpans toks els
  -- token t ends in el: first token! Make a TextSpan with token t as
  -- first token and find its end token. Call mkTextSpans again with
  -- token t as starting token and els.
  | fromMaybe 0 tEnd >= elStart && isJust tEnd
  = (mkTextSpan el
     (_token_id t)
     (findLastToken toks $ ((TP.getTextOffset el) + (TP.getTextLength el))))
    : mkTextSpans toks els
  -- -| otherwise = (mkTextSpan el (Just 6666) (Just 7777)) : mkTextSpans toks els
  where
    tEnd = _token_end t
    tStart = _token_start t
    elStart = TP.getTextOffset el
    elEnd = TP.getTextOffset el + TP.getTextLength el
mkTextSpans [] (el:els)
  -- This should not be needed. But we had non-exhaustive pattern
  -- errors before adding the guard, that checks for zero length
  -- element text.
  = (mkTextSpan el Nothing Nothing) : mkTextSpans [] els

findLastToken :: [Token] -> TextPosition -> Maybe Int
findLastToken [] _ = Nothing
findLastToken (t:[]) end
  | end >= fromMaybe 0 tStart && isJust tStart = _token_id t
  | otherwise = Nothing -- failure before
  where
    tStart = _token_start t
findLastToken (t:tn:ts) end
  | (fromMaybe 0 tnStart) < end && isJust tnStart = findLastToken (tn:ts) end
  | otherwise = _token_id t
  where
    tnStart = _token_start tn

-- * Arrows for writing the text structure layer.

writeTextStructureLayer :: (ArrowXml a) => Config     -- ^ the config
                        -> [TextStructure]            -- ^ the list of 'TextSpan'
                        -> a XmlTree XmlTree          -- ^ returns an xml arrow
writeTextStructureLayer cfg spans =
  let
     nsuri = _cfg_tcfTextCorpusNamespace cfg
     prefix = _cfg_tcfTokenIdPrefix cfg
     base = _cfg_tcfIdBase cfg
  in
    (mkqelem
     (mkNsName "textstructure" nsuri)
     [] -- empty attribute node
     (map (writeTextSpan nsuri prefix base) spans))
  where
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    maybeStrAttr n val = maybeToList $ fmap (sattr n) val
    writeTextSpan :: (ArrowXml a) => String -> String -> Int -> TextStructure -> a XmlTree XmlTree
    writeTextSpan ns pfx bs (TextSpan typ uri sTok eTok sText eText sSrc eSrc) =
      (mkqelem
        (mkNsName "textspan" ns)
        ((maybeStrAttr "type" typ) ++
         (maybeStrAttr "namespace" uri) ++
         (maybeStrAttr "start" $ idToBase pfx bs sTok) ++
         (maybeStrAttr "end" $ idToBase pfx bs eTok) ++
         (maybeAttr "textStart" sText) ++
         (maybeAttr "textEnd" eText) ++
         (maybeAttr "srcStart" sSrc) ++
         (maybeAttr "srcEnd" eSrc))
         [] -- no child nodes
      )
