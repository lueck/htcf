{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module HTCF.TcfData
  ( TcfData(..)
  , getToken
  , getTokenID
  , getLemma
  , fillTcfData
  , mkIdDataTuple
  , fromToken
  , tokensFromSentences
  , tokensFromPOStags
  , tokensFromLemmas
  , frequencies
  , updateWithFrequencies
  , collectTokenData
  ) where


import Data.Maybe
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import qualified Data.Aeson as A
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.HashMap.Lazy as HashMap
import Control.Lens

import HTCF.TcfLayers
import HTCF.Position
import HTCF.Range
import HTCF.Utils
import qualified HTCF.TokenLayer as T
import qualified HTCF.SentenceLayer as S
import qualified HTCF.POStagLayer as P
import qualified HTCF.LemmaLayer as L

-- * Type Defs

-- | This module defines the ADT 'TcfData' which collects data from
-- diverse tcf layers into its type constructors, eg. the token layer,
-- sentences, POStags and lemmas into 'TcfTokenData'.
data TcfData =
  TcfTokenData                  -- ^ A token and all the information
                                -- generated about it by weblicht
  { _tcftok_token :: Maybe String       -- ^ the token
  , _tcftok_tokenId :: Maybe Int        -- ^ the token's identifier
  , _tcftok_textStart :: Maybe TextPosition -- ^ the token's start pos in text layer
  , _tcftok_textEnd :: Maybe TextPosition -- ^ the token's end pos in text layer
  , _tcftok_srcStart :: Maybe XmlPosition -- ^ the token's start pos in source
  , _tcftok_srcEnd :: Maybe XmlPosition -- ^ the token's end pos in source
  , _tcftok_posTag :: Maybe String      -- ^ the POStag of the token
  , _tcftok_tagSet :: Maybe String      -- ^ the tag set of the POStag
  , _tcftok_lemma :: Maybe String       -- ^ the token's lemmatized string
  , _tcftok_sentenceId :: Maybe Int     -- ^ identifier of the token's sentence
  --, _tcftok_freqToken :: Maybe Int      -- ^ relative frequency of the token in the document
  --, _tcftok_freqLemma :: Maybe Int      -- ^ relative frequency of the lemma in the document
  }
  | TcfFrequency
  { _tcffreq_token :: Maybe String
  , _tcffreq_frequency :: Maybe Int
  } deriving (Eq, Show, Generic)

makeLenses ''TcfData

getTokenID :: TcfData -> Maybe Int
getTokenID (TcfTokenData _ tid _ _ _ _ _ _ _ _) = tid
{-# DEPRECATED getTokenID "Use lenses instead!" #-}

getToken :: TcfData -> Maybe String
getToken (TcfTokenData tok _ _ _ _ _ _ _ _ _) = tok
{-# DEPRECATED getToken "Use lenses instead!" #-}

getLemma :: TcfData -> Maybe String
getLemma (TcfTokenData _ _ _ _ _ _ _ _ lem _) = lem
{-# DEPRECATED getLemma "Use lenses instead!" #-}

-- * Exporting

instance A.ToJSON TcfData

instance Csv.ToRecord TcfData

instance Csv.ToRecord (PostgresRange TcfData) where
  toRecord (PostgresRange (TcfTokenData tok tid tStart tEnd sStart sEnd ptag tset lem sid))
    = Csv.record [ maybeToField B.empty tok
                 , maybeToField B.empty tid
                 , toPgRange tStart tEnd
                 , toPgRange sStart sEnd
                 , maybeToField B.empty ptag
                 , maybeToField B.empty tset
                 , maybeToField B.empty lem
                 , maybeToField B.empty sid
                 --, maybeToField B.empty fTok
                 --, maybeToField B.empty fLem
                 ]


-- * Merging parsed data to common algebraic data type TcfData

-- | Fill TcfData d1 with Just fields of TcfData d2. Just fields from d1 are not updated.
fillTcfData :: TcfData -> TcfData -> TcfData
fillTcfData
  (TcfTokenData t1 tid1 ts1 te1 ss1 se1 pt1 set1 l1 sid1)
  (TcfTokenData t2 tid2 ts2 te2 ss2 se2 pt2 set2 l2 sid2) =
  TcfTokenData
  { _tcftok_token = fillMaybe t1 t2
  , _tcftok_tokenId = fillMaybe tid1 tid2
  , _tcftok_textStart = fillMaybe ts1 ts2
  , _tcftok_textEnd = fillMaybe te1 te2
  , _tcftok_srcStart = fillMaybe ss1 ss2
  , _tcftok_srcEnd = fillMaybe se1 se2
  , _tcftok_posTag = fillMaybe pt1 pt2
  , _tcftok_tagSet = fillMaybe set1 set2
  , _tcftok_lemma = fillMaybe l1 l2
  , _tcftok_sentenceId = fillMaybe sid1 sid2
  --, _tcftok_freqToken = fillMaybe ft1 ft2
  --, _tcftok_freqLemma = fillMaybe fl1 fl2
  }
  where fillMaybe p1 p2
          | isJust p1 = p1
          | otherwise = p2

-- | Make a tuple (id, data) from data.
mkIdDataTuple :: TcfData -> (Int, TcfData)
mkIdDataTuple d@(TcfTokenData _ tid _ _ _ _ _ _ _ _) = (fromMaybe 0 tid, d)

-- * Conversion from layer data

-- | Make TcfTokenData from Token
fromToken :: T.Token -> TcfData
fromToken tok = TcfTokenData
  { _tcftok_token = Just $ T._token_token tok
  , _tcftok_tokenId = T._token_id tok
  , _tcftok_textStart = T._token_start tok
  , _tcftok_textEnd = T._token_end tok
  , _tcftok_srcStart = T._token_srcStart tok
  , _tcftok_srcEnd = T._token_srcEnd tok
  , _tcftok_posTag = Nothing
  , _tcftok_tagSet = Nothing
  , _tcftok_lemma = Nothing
  , _tcftok_sentenceId = Nothing
  --, _tcftok_freqToken = Nothing
  --, _tcftok_freqLemma = Nothing
  }

fromTokenFrequency' :: Maybe Int -> Maybe Int -> TcfData
fromTokenFrequency' tid freq = TcfTokenData
  { _tcftok_tokenId = tid
  , _tcftok_token = Nothing
  , _tcftok_textStart = Nothing
  , _tcftok_textEnd = Nothing
  , _tcftok_srcStart = Nothing
  , _tcftok_srcEnd = Nothing
  , _tcftok_posTag = Nothing
  , _tcftok_tagSet = Nothing
  , _tcftok_lemma = Nothing
  , _tcftok_sentenceId = Nothing
  --, _tcftok_freqToken = freq
  --, _tcftok_freqLemma = Nothing
  }

fromLemmaFrequency' :: Maybe Int -> Maybe Int -> TcfData
fromLemmaFrequency' tid freq = TcfTokenData
  { _tcftok_tokenId = tid
  , _tcftok_token = Nothing
  , _tcftok_textStart = Nothing
  , _tcftok_textEnd = Nothing
  , _tcftok_srcStart = Nothing
  , _tcftok_srcEnd = Nothing
  , _tcftok_posTag = Nothing
  , _tcftok_tagSet = Nothing
  , _tcftok_lemma = Nothing
  , _tcftok_sentenceId = Nothing
  --, _tcftok_freqToken = Nothing
  --, _tcftok_freqLemma = freq
  }

tokensFromPOStags :: [P.POStag] -> [TcfData]
tokensFromPOStags =
  foldl (\acc tag -> acc ++ (toksFromPOStag tag)) []
  where
    toksFromPOStag tg =
      map (\i -> TcfTokenData
            { _tcftok_tokenId = Just i
            , _tcftok_token = Nothing
            , _tcftok_textStart = Nothing
            , _tcftok_textEnd = Nothing
            , _tcftok_srcStart = Nothing
            , _tcftok_srcEnd = Nothing
            , _tcftok_posTag = Just $ P._postag_posTag tg
            , _tcftok_tagSet = P._postag_tagSet tg
            , _tcftok_lemma = Nothing
            , _tcftok_sentenceId = Nothing
            --, _tcftok_freqToken = Nothing
            --, _tcftok_freqLemma = Nothing
            }) $ P._postag_tokenIDs tg
      
tokensFromSentences :: [S.Sentence] -> [TcfData]
tokensFromSentences =
  foldl (\acc sentence -> acc ++ (toksFromSentence sentence)) []
  where
    toksFromSentence s =
      map (\i -> TcfTokenData
            { _tcftok_tokenId = Just i
            , _tcftok_token = Nothing
            , _tcftok_textStart = Nothing
            , _tcftok_textEnd = Nothing
            , _tcftok_srcStart = Nothing
            , _tcftok_srcEnd = Nothing
            , _tcftok_posTag = Nothing
            , _tcftok_tagSet = Nothing
            , _tcftok_lemma = Nothing
            , _tcftok_sentenceId = S._sentence_id s
            --, _tcftok_freqToken = Nothing
            --, _tcftok_freqLemma = Nothing
            }) $ S._sentence_tokens s
      
tokensFromLemmas :: [L.Lemma] -> [TcfData]
tokensFromLemmas =
  foldl (\acc lem -> acc ++ (toksFromLemma lem)) []
  where
    toksFromLemma l =
      map (\i -> TcfTokenData
            { _tcftok_tokenId = Just i
            , _tcftok_token = Nothing
            , _tcftok_textStart = Nothing
            , _tcftok_textEnd = Nothing
            , _tcftok_srcStart = Nothing
            , _tcftok_srcEnd = Nothing
            , _tcftok_posTag = Nothing
            , _tcftok_tagSet = Nothing
            , _tcftok_lemma = Just $ L._lemma_lemma l
            , _tcftok_sentenceId = Nothing
            --, _tcftok_freqToken = Nothing
            --, _tcftok_freqLemma = Nothing
            }) $ L._lemma_tokenIDs l


-- * Statistics

frequencies :: (TcfData -> Maybe String) -- ^ Getter for the field (token, lemma) of the tokens record
            -> [TcfData] -- ^ the tokens
            -> [TcfData] -- ^ returned list of frequencies
frequencies fieldGetter toks =
  map (\(tok, freq) -> TcfFrequency { _tcffreq_token = Just tok, _tcffreq_frequency = Just freq}) $ HashMap.toList freqs
  where
    fieldGetter' :: TcfData -> String
    fieldGetter' = (fromMaybe "") . fieldGetter
    freqs :: (HashMap.HashMap String Int)
    freqs = HashMap.fromListWith (+) tuples
    tuples = map (\tok -> ((fieldGetter' tok), 1)) toks
    

-- | Count frequencies of the values of a field of the TcfTokenData
-- given in an integer mapping. Returns a mapping with frequency
-- fields updated.
updateWithFrequencies :: (TcfData -> Maybe String) -- ^ Getter for the field (token, lemma) of the tokens revcord
                      -> (Maybe Int -> Maybe Int -> TcfData) -- ^ Constructor for a TcfData taking an tokenID and a frequency
                      -> (IntMap.IntMap TcfData) -- ^ an integer mapping of TcfData
                      -> (IntMap.IntMap TcfData)
updateWithFrequencies fieldGetter constructor mapping =
  IntMap.map fillWithFreq mapping
  where
    fieldGetter' :: TcfData -> String
    fieldGetter' = (fromMaybe "") . fieldGetter
    fillWithFreq tok = fillTcfData tok (constructor (getTokenID tok) (HashMap.lookup (fieldGetter' tok) freqs))
    freqs :: (HashMap.HashMap String Int)
    freqs = HashMap.fromListWith (+) tuples
    tuples = map (\tok -> ((fieldGetter' tok), 1)) $ IntMap.elems mapping


-- * Collecting layer data

-- | This takes an uncurried tuple of the kind
-- 'HTCF.ReadTcf.runTcfReader' returns. The data of the tcf layers is
-- collected into a list of 'TcfTokenData'.
--
-- We use an interger key mapping from Data.IntMap.Lazy. It uses a
-- tree representation internally. There is no risc of worst-case
-- complexity O(min(n, W)) when inserting tokens ordered by keys,
-- because it uses a big-endian patricia-tree.
collectTokenData :: TcfLayers -> [TcfData]
collectTokenData layers =
  IntMap.elems $
  --updateWithFrequencies getLemma fromLemmaFrequency' $
  --updateWithFrequencies getToken fromTokenFrequency' $
  updateMapWith (tokensFromSentences $ _layers_sentences layers) $
  updateMapWith (tokensFromLemmas $ _layers_lemmas layers) $
  updateMapWith (tokensFromPOStags $ _layers_posTags layers) $
  IntMap.fromList $ map (mkIdDataTuple . fromToken) $ _layers_tokens layers
  where
    updateMapWith :: [TcfData] -> IntMap.IntMap TcfData -> IntMap.IntMap TcfData
    updateMapWith tcfs mapping = foldl (\acc (tid, tok) -> IntMap.adjust (flip fillTcfData tok) tid acc) mapping $ map mkIdDataTuple tcfs
