{-# LANGUAGE DisambiguateRecordFields #-}
module HTCF.LayerTypeDefs
  ( Text (..)
  , Token (..)
  , TokenID
  , Sentence (..)
  , SentenceID
  , Lemma (..)
  , LemmaID
  ) where

import HTCF.Position

-- | Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Annotation_Layers

-- | Represents the text layer the text.
data Text = Text String
  deriving (Show, Eq)

-- * Token

type TokenID = Int

-- | Represents a single token
data Token = Token                      
  { token :: String                   -- ^ the token
  , tokenId :: Maybe TokenID          -- ^ the token's ID
  , start :: Maybe TextPosition       -- ^ start character offset
                                      -- position in relation to text
                                      -- layer
  , end :: Maybe TextPosition         -- ^ end character offset
                                      -- position in relation to text
                                      -- layer
  , srcStart :: Maybe XmlPosition     -- ^ start character offset
                                      -- position in relation to XML
                                      -- source file
  , srcEnd :: Maybe XmlPosition       -- ^ end character offset
                                      -- position in relation to XML
                                      -- source file
    } deriving (Show, Eq)

-- * Sentence

type SentenceID = Int

-- | Represents a single sentence in the sentence boundary annotation.
data Sentence = Sentence
  { sentenceId :: Maybe SentenceID    -- ^ the sentence's ID
  , sentenceTokenIDs :: [TokenID]     -- ^ list of token IDs
  }

-- * Lemma

type LemmaID = Int

-- | Represents a lemma annotation on tokens.
data Lemma = Lemma
    { lemmaId :: Maybe LemmaID        -- ^ the lemma's ID
    , lemma :: String                 -- ^ the lemma string
    , lemmaTokenIDs :: [TokenID]      -- ^ reference to tokens
    }

{-
data POSTags = POSTags
  { tagset :: PartOfSpeechTagSet
  , tags :: [POSTag]
  } deriving (Show)

-- | annotation of a token with part-of-speech tag.
data POSTag = POSTag
  { id :: POStagID
  , tag :: PartOfSpeechTag
  , tokenID :: TokenID
  } deriving (Eq, Show)
-}
