{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
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
  )  where


import Data.Maybe
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import qualified Data.Aeson as A
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.HashMap.Lazy as HashMap

import HTCF.TcfLayers
import HTCF.Position
import HTCF.Range
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
  { token :: Maybe String       -- ^ the token
  , tokenId :: Maybe Int        -- ^ the token's identifier
  , textStart :: Maybe TextPosition -- ^ the token's start pos in text layer
  , textEnd :: Maybe TextPosition -- ^ the token's end pos in text layer
  , srcStart :: Maybe XmlPosition -- ^ the token's start pos in source
  , srcEnd :: Maybe XmlPosition -- ^ the token's end pos in source
  , posTag :: Maybe String      -- ^ the POStag of the token
  , tagSet :: Maybe String      -- ^ the tag set of the POStag
  , lemma :: Maybe String       -- ^ the token's lemmatized string
  , sentenceId :: Maybe Int     -- ^ identifier of the token's sentence
  --, freqToken :: Maybe Int      -- ^ relative frequency of the token in the document
  --, freqLemma :: Maybe Int      -- ^ relative frequency of the lemma in the document
  }
  | TcfFrequency
  { token :: Maybe String
  , frequency :: Maybe Int
  } deriving (Eq, Show, Generic)


getTokenID :: TcfData -> Maybe Int
getTokenID (TcfTokenData _ tid _ _ _ _ _ _ _ _) = tid

getToken :: TcfData -> Maybe String
getToken (TcfTokenData tok _ _ _ _ _ _ _ _ _) = tok

getLemma :: TcfData -> Maybe String
getLemma (TcfTokenData _ _ _ _ _ _ _ _ lem _) = lem

-- * Exporting

instance A.ToJSON TcfData

instance Csv.ToRecord TcfData

instance Csv.ToRecord (PostgresRange TcfData) where
  toRecord (PostgresRange (TcfTokenData tok tid tStart tEnd sStart sEnd ptag tset lem sid))
    = Csv.record [ toField' B.empty tok
                 , toField' B.empty tid
                 , (B.concat [ "[", (toField' "NULL" tStart), ","
                             , (toField' "NULL" tEnd), "]"])
                 , (B.concat [ "[", (toField' "NULL" sStart), ","
                             , (toField' "NULL" sEnd), "]"])
                 , toField' B.empty ptag
                 , toField' B.empty tset
                 , toField' B.empty lem
                 , toField' B.empty sid
                 --, toField' B.empty fTok
                 --, toField' B.empty fLem
                 ]

toField' :: (Csv.ToField a) => B.ByteString -- ^ Default value
         -> Maybe a -- ^ the maybe field, 
         -> Csv.Field
toField' deflt f = maybe deflt Csv.toField f


-- * Merging parsed data to common algebraic data type TcfData

-- | Fill TcfData d1 with Just fields of TcfData d2. Just fields from d1 are not updated.
fillTcfData :: TcfData -> TcfData -> TcfData
fillTcfData
  (TcfTokenData t1 tid1 ts1 te1 ss1 se1 pt1 set1 l1 sid1)
  (TcfTokenData t2 tid2 ts2 te2 ss2 se2 pt2 set2 l2 sid2) =
  TcfTokenData
  { token = fillMaybe t1 t2
  , tokenId = fillMaybe tid1 tid2
  , textStart = fillMaybe ts1 ts2
  , textEnd = fillMaybe te1 te2
  , srcStart = fillMaybe ss1 ss2
  , srcEnd = fillMaybe se1 se2
  , posTag = fillMaybe pt1 pt2
  , tagSet = fillMaybe set1 set2
  , lemma = fillMaybe l1 l2
  , sentenceId = fillMaybe sid1 sid2
  --, freqToken = fillMaybe ft1 ft2
  --, freqLemma = fillMaybe fl1 fl2
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
  { token = Just $ T.getToken tok
  , tokenId = T.getTokenID tok
  , textStart = T.getTokenStartTextPos tok
  , textEnd = T.getTokenEndTextPos tok
  , srcStart = T.getTokenStartSrcPos tok
  , srcEnd = T.getTokenEndSrcPos tok
  , posTag = Nothing
  , tagSet = Nothing
  , lemma = Nothing
  , sentenceId = Nothing
  --, freqToken = Nothing
  --, freqLemma = Nothing
  }

fromTokenFrequency' :: Maybe Int -> Maybe Int -> TcfData
fromTokenFrequency' tid freq = TcfTokenData
  { tokenId = tid
  , token = Nothing
  , textStart = Nothing
  , textEnd = Nothing
  , srcStart = Nothing
  , srcEnd = Nothing
  , posTag = Nothing
  , tagSet = Nothing
  , lemma = Nothing
  , sentenceId = Nothing
  --, freqToken = freq
  --, freqLemma = Nothing
  }

fromLemmaFrequency' :: Maybe Int -> Maybe Int -> TcfData
fromLemmaFrequency' tid freq = TcfTokenData
  { tokenId = tid
  , token = Nothing
  , textStart = Nothing
  , textEnd = Nothing
  , srcStart = Nothing
  , srcEnd = Nothing
  , posTag = Nothing
  , tagSet = Nothing
  , lemma = Nothing
  , sentenceId = Nothing
  --, freqToken = Nothing
  --, freqLemma = freq
  }

tokensFromPOStags :: [P.POStag] -> [TcfData]
tokensFromPOStags =
  foldl (\acc tag -> acc ++ (toksFromPOStag tag)) []
  where
    toksFromPOStag tg =
      map (\i -> TcfTokenData
            { tokenId = Just i
            , token = Nothing
            , textStart = Nothing
            , textEnd = Nothing
            , srcStart = Nothing
            , srcEnd = Nothing
            , posTag = Just $ P.getPOStag tg
            , tagSet = P.getPOStagTagSet tg
            , lemma = Nothing
            , sentenceId = Nothing
            --, freqToken = Nothing
            --, freqLemma = Nothing
            }) $ P.getPOStagTokenIDs tg
      
tokensFromSentences :: [S.Sentence] -> [TcfData]
tokensFromSentences =
  foldl (\acc sentence -> acc ++ (toksFromSentence sentence)) []
  where
    toksFromSentence s =
      map (\i -> TcfTokenData
            { tokenId = Just i
            , token = Nothing
            , textStart = Nothing
            , textEnd = Nothing
            , srcStart = Nothing
            , srcEnd = Nothing
            , posTag = Nothing
            , tagSet = Nothing
            , lemma = Nothing
            , sentenceId = S.getSentenceID s
            --, freqToken = Nothing
            --, freqLemma = Nothing
            }) $ S.getTokens s
      
tokensFromLemmas :: [L.Lemma] -> [TcfData]
tokensFromLemmas =
  foldl (\acc lem -> acc ++ (toksFromLemma lem)) []
  where
    toksFromLemma l =
      map (\i -> TcfTokenData
            { tokenId = Just i
            , token = Nothing
            , textStart = Nothing
            , textEnd = Nothing
            , srcStart = Nothing
            , srcEnd = Nothing
            , posTag = Nothing
            , tagSet = Nothing
            , lemma = Just $ L.getLemma l
            , sentenceId = Nothing
            --, freqToken = Nothing
            --, freqLemma = Nothing
            }) $ L.getLemmaTokenIDs l


-- * Statistics

frequencies :: (TcfData -> Maybe String) -- ^ Getter for the field (token, lemma) of the tokens record
            -> [TcfData] -- ^ the tokens
            -> [TcfData] -- ^ returned list of frequencies
frequencies fieldGetter toks =
  map (\(tok, freq) -> TcfFrequency { token = Just tok, frequency = Just freq}) $ HashMap.toList freqs
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
  updateMapWith (tokensFromSentences $ getSentences layers) $
  updateMapWith (tokensFromLemmas $ getLemmas layers) $
  updateMapWith (tokensFromPOStags $ getPOStags layers) $
  IntMap.fromList $ map (mkIdDataTuple . fromToken) $ getTokens layers
  where
    updateMapWith :: [TcfData] -> IntMap.IntMap TcfData -> IntMap.IntMap TcfData
    updateMapWith tcfs mapping = foldl (\acc (tid, tok) -> IntMap.adjust (flip fillTcfData tok) tid acc) mapping $ map mkIdDataTuple tcfs
