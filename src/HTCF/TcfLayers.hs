module HTCF.TcfLayers
  ( TcfLayers(..)
  , getTextLayer
  , getTokens
  , getSentences
  , getPOStags
  , getLemmas
  ) where

import qualified HTCF.TokenLayer as T
import qualified HTCF.SentenceLayer as S
import qualified HTCF.POStagLayer as P
import qualified HTCF.LemmaLayer as L
import qualified HTCF.TextLayer as Tx


-- | An ADT that collects all the layers from TCF.
data TcfLayers = TcfLayers
  { text :: [Tx.Text]
  , tokens:: [T.Token]
  , sentences :: [S.Sentence]
  , posTags :: [P.POStag]
  , lemmas :: [L.Lemma]
  } deriving (Eq, Show)


-- * getting the record fields of 'TcfLayers'

getTextLayer :: TcfLayers -> [Tx.Text]
getTextLayer (TcfLayers t _ _ _ _) = t

getTokens :: TcfLayers -> [T.Token]
getTokens (TcfLayers _ toks _ _ _) = toks

getSentences :: TcfLayers -> [S.Sentence]
getSentences (TcfLayers _ _ s _ _) = s

getPOStags :: TcfLayers -> [P.POStag]
getPOStags (TcfLayers _ _ _ ts _) = ts

getLemmas :: TcfLayers -> [L.Lemma]
getLemmas (TcfLayers _ _ _ _ ls) = ls

