module HTCF.ReadTcf
  ( runTcfReader
  , parseTcfLayers
  , parseTokens
  , parseToken
  ) where

import Text.XML.HXT.Core

import HTCF.LayerTypeDefs
import HTCF.Utils

import HTCF.ConfigParser

runTcfReader :: [Config] -> FilePath -> IO [{- FIXME: -}Token]
runTcfReader cfg fname = do
  layers <- runX (readDocument [withValidate no] fname >>>
                  propagateNamespaces //>
                  --isElem >>>
                  --hasQName (mkNsName "TextCorpus" $ getTcfTextCorpusNamespace cfg) >>>
                  multi (parseTcfLayers cfg))
  return layers


parseTcfLayers :: [Config] -> IOSArrow XmlTree {- FIXME: -}Token
parseTcfLayers cfg =
  parseTokens cfg

parseTokens :: [Config] -> IOSArrow XmlTree Token
parseTokens cfg =
  isElem >>> hasQName (mkNsName "tokens" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  isElem >>> parseToken cfg

parseToken :: [Config] -> IOSArrow XmlTree Token
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
