{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module HTCF.LemmaLayer
  ( Lemma (..)
  , getLemma
  , getLemmaTokenIDs
  , parseLemmas
  , parseLemma
  , writeLemmaLayer
  ) where

import Text.XML.HXT.Core
import Data.Maybe
import Data.List
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import qualified Data.Aeson as A

import HTCF.ConfigParser
import HTCF.Utils
import HTCF.ArrowXml
import HTCF.Range

-- | This modules defines types and functions for reading and writing
-- the Lemma layer.
-- Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Lemmas
--
-- The tokenizer is in Tokenizer.hs.


-- * Lemma type defs

-- | Represents a single Lemma
data Lemma = Lemma                      
  { lemma :: String                   -- ^ the Lemma
  , tokenIDs :: [Int]                  -- ^ the token IDs
  } deriving (Show, Eq, Generic)

-- * Exporting

-- | 'Lemma' is ready to be exported to JSON.
instance A.ToJSON Lemma

-- | 'Lemma' is ready to be exported to CSV.
instance Csv.ToRecord Lemma where
  toRecord (Lemma l ids)
    = Csv.record [ Csv.toField l
                 , Csv.toField $ intercalate " " $ map show ids
                 ]

-- | 'Lemma' is ready to be exported to CSV with text and source
-- offsets formatted as PostgreSQL's range type.
instance Csv.ToRecord (PostgresRange Lemma) where
  toRecord (PostgresRange l)
    = Csv.toRecord l

-- * Getters for the fields of the 'Lemma' record.

getLemma :: Lemma -> String
getLemma (Lemma l _) = l

getLemmaTokenIDs :: Lemma -> [Int]
getLemmaTokenIDs (Lemma _ ids) = ids

-- * Arrows for reading a tcf lemma layer.

parseLemmas :: (ArrowXml a) => [Config] -> Int -> Int -> a XmlTree Lemma
parseLemmas cfg pfxLen base =
  --traceMsg 1 ("Parsing Lemma layer with prefix length " ++ (show pfxLen) ++ " and base " ++ (show base)) >>> 
  isElem >>> hasQNameCase (mkNsName "lemmas" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  parseLemma cfg pfxLen base

parseLemma :: (ArrowXml a) => [Config] -> Int -> Int -> a XmlTree Lemma
parseLemma cfg pfxLen base =
  hasQNameCase (mkNsName "lemma" $ getTcfTextCorpusNamespace cfg) >>>
  (getChildren >>> getText) &&&
  getAttrCaseValue "tokenIDs" >>>
  arr (\(lem, ids) ->
         (Lemma
           lem
           (parseIDs pfxLen base ids)))
{-# INLINE parseLemma #-}

-- * Arrows for writing the tcf lemma layer.

-- | Arrow for writing the lemma layer.
writeLemmaLayer :: (ArrowXml a) => [Config] -- ^ the config
                -> [Lemma]                  -- ^ the list of lemmas
                -> a XmlTree XmlTree        -- ^ returns an xml arrow
writeLemmaLayer cfg ts =
  let base = getTcfIdBase cfg
      prefix = getTcfTokenIdPrefix cfg
      ns = getTcfTextCorpusNamespace cfg in
    (mkqelem
     (mkNsName "lemmas" ns) -- qname
     [] -- attribute nodes
     (map (writeLemma ns prefix base) ts))
  where
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    maybeStrAttr n val = maybeToList $ fmap (sattr n) val
    writeLemma :: (ArrowXml a) => String -> String -> Int -> Lemma -> a XmlTree XmlTree
    writeLemma nsuri pfx bs (Lemma t ids) =
      (mkqelem
       (mkNsName "lemma" nsuri)
       (maybeAttr "tokenIDs" $ Just $ concatMap ((++ " ") . (fromMaybe "") . (idToBase pfx bs) . Just) ids)
       [(txt t)])
