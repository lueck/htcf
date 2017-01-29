module HTCF.WriteTcf
  ( writeTokenLayer
  ) where

import Text.XML.HXT.Core
import Data.Maybe

import HTCF.Layers

import Text.XML.TCF.Parser.ConfigParser

writeTokenLayer :: (ArrowXml a) => [Config] -> [Layer] -> a XmlTree XmlTree
writeTokenLayer cfg ts =
  (mkqelem
   (mkNsName "tokens" ns) -- qname
   [] -- attribute nodes
   (map writeToken ts))
  where
    ns = getTcfTextCorpusNamespace cfg
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    writeToken :: (ArrowXml a) => Layer -> a XmlTree XmlTree
    writeToken (Token t idd start end sStart sEnd) =
      (mkqelem
       (mkNsName "token" ns)
       ((maybeAttr "id" idd) ++ (maybeAttr "start" start) ++ (maybeAttr "end" end) ++ (maybeAttr "srcStart" sStart) ++ (maybeAttr "srcEnd" sEnd))
       [(txt t)])
