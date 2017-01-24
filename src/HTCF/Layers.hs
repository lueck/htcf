{-# LANGUAGE DisambiguateRecordFields #-}
module HTCF.Layers
  ( Layer (..)
  , TokenID
  , SentenceID
  , LemmaID
  ) where

import Text.XML.TCF.Parser.Position

type TokenID = Int
type SentenceID = Int
type LemmaID = Int

-- | @Layer@ is a record for representing the TCF's annotation layers
-- in a homogenous haskell list. There is a constructor for each type
-- of annotation layer. A TCF-file can be read to @[Layer]@, and a
-- @[Layer]@ list can be written to a file.
--
-- Each record in the list will represent a single item of a layer,
-- e.g. for every <token> in <tokens> there will be a 'Token' record
-- in the list. Since the TCF is designed to have a single <text> tag
-- for the text layer, there will be a single 'Text' record in the
-- list.
--
-- Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Annotation_Layers
data Layer
  = Text                                -- ^ Represents the text layer
    { text :: String                    -- ^ the text
    }
  | Token                               -- ^ Represents a single token
    { token :: String                   -- ^ the token
    , tokenId :: Maybe TokenID               -- ^ the token's ID
    , textStart :: Maybe TextPosition   -- ^ start character offset
                                        -- position in relation to
                                        -- text layer
    , textEnd :: Maybe TextPosition     -- ^ end character offset
                                        -- position in relation to
                                        -- text layer
    , srcStart :: Maybe XmlPosition     -- ^ start character offset
                                        -- position in relation to XML
                                        -- source file
    , srcEnd :: Maybe XmlPosition       -- ^ end character offset
                                        -- position in relation to XML
                                        -- source file
    }
  | Sentence                            -- ^ Represents a single
                                        -- sentence in the sentence
                                        -- boundary annotation.
    { sentenceId :: Maybe SentenceID    -- ^ the sentence's ID
    , sentenceTokenIDs :: [TokenID]             -- ^ list of token IDs
    }
  | Lemma                               -- ^ Represents a lemma annotation on tokens.
    { lemmaId :: Maybe LemmaID          -- ^ the lemma's ID
    , lemma :: String                   -- ^ the lemma string
    , lemmaTokenIDs :: [TokenID]             -- ^ reference to tokens
    }
    {-
    | POStagSet                         -- ^ The tag set of the part-of-speech tag annotation
      { tagset :: PartOfSpeechTagSet
      }
    | POStag                            -- ^ annotation of a token with part-of-speech tag
      { id :: POStagID
      , tag :: PartOfSpeechTag
      , tokenID :: TokenID
      } -}
  deriving Show


