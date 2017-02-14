module HTCF.ReadTcf
  ( runTcfReader
  ) where

import Text.XML.HXT.Core

import HTCF.ConfigParser

import HTCF.TokenLayer
import HTCF.TextLayer

-- | Run the TCF reader in the IO monad. Use this as an example for
-- your parser with extra layers.
runTcfReader :: [Config] -> FilePath -> IO ([Text], [Token] {-, [SomethingElse]-})
runTcfReader cfg fname = do
  tree <- runX (readDocument [withValidate no] fname >>>
                propagateNamespaces
               )
  (tokenIdPfx, idBase) <- guessAboutTokenId cfg tree
  text <- runX (constL tree //>
                parseTextLayer cfg)
  tokens <- runX (constL tree //>
                  parseTokens cfg tokenIdPfx idBase)
  {-somethingElse <- runX (constL tree //>
                           parseSomethingElse)-}
  return (text, tokens {-, somethingElse -})
