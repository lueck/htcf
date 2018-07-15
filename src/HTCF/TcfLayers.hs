{-# LANGUAGE TemplateHaskell #-}

module HTCF.TcfLayers
  ( TcfLayers(..)
  , getTextLayer
  , getTokens
  , getSentences
  , getPOStags
  , getLemmas
  ) where

import Control.Lens

import qualified HTCF.TokenLayer as T
import qualified HTCF.SentenceLayer as S
import qualified HTCF.POStagLayer as P
import qualified HTCF.LemmaLayer as L
import qualified HTCF.TextLayer as Tx


-- | An ADT that collects all the layers from TCF.
data TcfLayers = TcfLayers
  { _layers_text :: [Tx.Text]
  , _layers_tokens:: [T.Token]
  , _layers_sentences :: [S.Sentence]
  , _layers_posTags :: [P.POStag]
  , _layers_lemmas :: [L.Lemma]
  } deriving (Eq, Show)

makeLenses ''TcfLayers

-- * getting the record fields of 'TcfLayers'

getTextLayer :: TcfLayers -> [Tx.Text]
getTextLayer (TcfLayers t _ _ _ _) = t
{-# DEPRECATED getTextLayer "Use lenses instead of getTextLayer!" #-}

getTokens :: TcfLayers -> [T.Token]
getTokens (TcfLayers _ toks _ _ _) = toks
{-# DEPRECATED getTokens "Use lenses instead of getTokens!" #-}

getSentences :: TcfLayers -> [S.Sentence]
getSentences (TcfLayers _ _ s _ _) = s
{-# DEPRECATED getSentences "Use lenses instead of getSentences!" #-}

getPOStags :: TcfLayers -> [P.POStag]
getPOStags (TcfLayers _ _ _ ts _) = ts
{-# DEPRECATED getPOStags "Use lenses instead of getPOStags!" #-}

getLemmas :: TcfLayers -> [L.Lemma]
getLemmas (TcfLayers _ _ _ _ ls) = ls
{-# DEPRECATED getLemmas "Use lenses instead of getLemmas!" #-}
