module HTCF.WriteTcf
  ( writeTokenLayer
  , runTcfWriter
  ) where

import Text.XML.HXT.Core
import Data.Maybe

import HTCF.LayerTypeDefs

import HTCF.ConfigParser

-- Does not really make sense here. Put it in the command line
-- program.
runTcfWriter :: [Config] -> [{- FIXME: -}Token] -> IO (String)
runTcfWriter cfg toks = do
  rc <- runX (root [] [(writeTokenLayer cfg toks)] >>>
              writeDocumentToString [withIndent yes])
  return $ concat rc


-- | Arrow for writing the token layer.
writeTokenLayer :: (ArrowXml a) => [Config] -- ^ the config
                -> [Token]                  -- ^ the list of tokens
                -> a XmlTree XmlTree        -- ^ returns an xml arrow
writeTokenLayer cfg ts =
  (mkqelem
   (mkNsName "tokens" ns) -- qname
   [] -- attribute nodes
   (map writeToken ts))
  where
    ns = getTcfTextCorpusNamespace cfg
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    writeToken :: (ArrowXml a) => Token -> a XmlTree XmlTree
    writeToken (Token t idd start end sStart sEnd) =
      (mkqelem
       (mkNsName "token" ns)
       ((maybeAttr "id" idd) ++ (maybeAttr "start" start) ++ (maybeAttr "end" end) ++ (maybeAttr "srcStart" sStart) ++ (maybeAttr "srcEnd" sEnd))
       [(txt t)])
