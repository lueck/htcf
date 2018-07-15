module HTCF.ReadTcf
  ( runTcfReader
  , runTcfLayerReader
  ) where

import Text.XML.HXT.Core

import HTCF.Config

import HTCF.TokenLayer
import HTCF.TextLayer
import HTCF.SentenceLayer
import HTCF.POStagLayer
import HTCF.LemmaLayer
import qualified HTCF.TcfLayers as Ls

-- | Run the TCF reader in IO. Use this as an example for
-- your parser with extra layers.
runTcfReader :: Config -> FilePath -> IO Ls.TcfLayers
runTcfReader cfg fname = do
  tree <- runX (readDocument [withValidate no] fname >>>
                propagateNamespaces
               )
  (tokenIdPfx, tokenIdBase) <- guessAboutTokenId cfg tree
  (sentenceIdPfx, sentenceIdBase) <- guessAboutSentenceId cfg tree
  text <- runX (constL tree //>
                parseTextLayer cfg)
  toks <- runX (constL tree //>
                  parseTokens cfg tokenIdPfx tokenIdBase)
  sentences <- runX (constL tree //>
                    parseSentences cfg sentenceIdPfx sentenceIdBase tokenIdPfx tokenIdBase)
  posTags <- runX (constL tree //>
                  parsePOStags cfg tokenIdPfx tokenIdBase)
  lemmas <- runX (constL tree //>
                 parseLemmas cfg tokenIdPfx tokenIdBase)
  {-somethingElse <- runX (constL tree //>
                           parseSomethingElse)-}
  return Ls.TcfLayers { Ls._layers_text=text
                      , Ls._layers_tokens=toks
                      , Ls._layers_sentences=sentences
                      , Ls._layers_posTags=posTags
                      , Ls._layers_lemmas=lemmas
                      {-, somethingElse -}
                      }

-- | Run a parser arrow on a parsed xml tree and return the results of
-- the arrow wrapped in the IO monad.
runTcfLayerReader :: (Tree a)
                  => [a c] -- ^ the xml tree
                  -> Config -- ^ configuration
                  -> Int -- ^ prefix length of token IDs
                  -> Int -- ^ base of token IDs
                  -> (Config -> Int -> Int -> IOSLA (XIOState ()) (a c) b) -- ^ parser arrow
                  -> IO [b] -- ^ returns [b] IO action
runTcfLayerReader tree cfg tokenIdPfx tokenIdBase parser = do
  rc <- runX (constL tree //> parser cfg tokenIdPfx tokenIdBase)
  return rc
