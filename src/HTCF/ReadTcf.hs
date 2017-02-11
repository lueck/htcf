module HTCF.ReadTcf
  ( runTcfReader
  , parseTcfLayers
  ) where

import Text.XML.HXT.Core

import HTCF.ConfigParser

import HTCF.TokenLayer
import HTCF.TextLayer

-- | Run the TCF reader in the IO monad.
runTcfReader :: [Config] -> FilePath -> IO ([Text], [Token] {-, [SomethingElse]-})
runTcfReader cfg fname = do
  layers <- runX (readDocument [withValidate no] fname >>>
                  propagateNamespaces //>
                  parseTcfLayers cfg)
  return $ unzip{-3-} layers -- increment tier of unzip for something else

-- | The parser arrow for the TCF reader. Usage: see 'runTcfReader'.
parseTcfLayers :: (ArrowXml a) => [Config] -> a XmlTree (Text, Token {-, SomethingElse-})
parseTcfLayers cfg =
  isElem >>>
  hasQName (mkNsName "TextCorpus" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  parseTextLayer cfg &&&
  parseTokens cfg {- &&&
  parseSomethingElse -}
