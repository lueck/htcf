module HTCF.ReadTcf
  ( runTcfReader
  ) where

import Text.XML.HXT.Core

import HTCF.ConfigParser

import HTCF.TokenLayer
import HTCF.TextLayer
import HTCF.SentenceLayer
import HTCF.POStagLayer
import HTCF.LemmaLayer
import qualified HTCF.TcfLayers as Ls

-- | Run the TCF reader in IO. Use this as an example for
-- your parser with extra layers.
runTcfReader :: [Config] -> FilePath -> IO Ls.TcfLayers
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
  return Ls.TcfLayers { Ls.text=text
                      , Ls.tokens=toks
                      , Ls.sentences=sentences
                      , Ls.posTags=posTags
                      , Ls.lemmas=lemmas
                      {-, somethingElse -}
                      }
