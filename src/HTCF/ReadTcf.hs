module HTCF.ReadTcf
  ( runTcfReader
  , parseTcfLayers
  , parseTokens
  , parseToken
  ) where

import Text.XML.HXT.Core

import HTCF.Layers
import HTCF.Utils

import Text.XML.TCF.Parser.ConfigParser

runTcfReader :: [Config] -> FilePath -> IO [Layer]
runTcfReader cfg fname = do
  layers <- runX (readDocument [withValidate no] fname >>>
                  propagateNamespaces //>
                  --isElem >>>
                  --hasQName (mkNsName "TextCorpus" $ getTcfTextCorpusNamespace cfg) >>>
                  multi (parseTcfLayers cfg))
  return layers

parseTcfLayers :: [Config] -> IOSArrow XmlTree Layer
parseTcfLayers cfg =
  parseTokens cfg

parseTokens :: [Config] -> IOSArrow XmlTree Layer
parseTokens cfg =
  isElem >>> hasQName (mkNsName "tokens" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  isElem >>> parseToken cfg

parseToken :: [Config] -> IOSArrow XmlTree Layer
parseToken cfg =
  hasQName (mkNsName "token" $ getTcfTextCorpusNamespace cfg) >>>
  (getChildren >>> getText) &&&
  getAttrValue "ID" &&&
  getAttrValue "start" &&&
  getAttrValue "end" &&&
  getAttrValue "srcStart" &&&
  getAttrValue "srcEnd" >>>
  arr (\(t, (idd, (s, (e, (sS, sE))))) ->
         (Token
           t
           (readBasePrefixed cfg idd)
           (readIntMaybe $ Just s)
           (readIntMaybe $ Just e)
           (readIntMaybe $ Just sS)
           (readIntMaybe $ Just sE)))
{-# INLINE parseToken #-}
