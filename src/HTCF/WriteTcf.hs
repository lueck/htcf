module HTCF.WriteTcf
  ( writeTokens
  , writeToken
  ) where

import Text.XML.HXT.Core
import Data.Maybe

import HTCF.Layers

import Text.XML.TCF.Parser.ConfigParser

writeTokens :: (ArrowXml a) => [Config] -> [Layer] -> a XmlTree XmlTree
writeTokens cfg ts =
  (mkqelem
   (mkNsName "tokens" $ getTcfTextCorpusNamespace cfg) -- qname
   [] -- attribute nodes
   (map (writeToken cfg) ts))

writeToken :: (ArrowXml a) => [Config] -> Layer -> a XmlTree XmlTree
writeToken cfg (Token t idd start end sStart sEnd) =
  (mkqelem
   (mkNsName "token" ns)
   ((maybeAttr "id" idd) ++ (maybeAttr "start" start) ++ (maybeAttr "end" end) ++ (maybeAttr "srcStart" sStart) ++ (maybeAttr "srcEnd" sEnd))
   [(txt t)])
  where
    ns = getTcfTextCorpusNamespace cfg
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
