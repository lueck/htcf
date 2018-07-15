{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module HTCF.POStagLayer
  ( POStag (..)
  , getPOStag
  , getPOStagTokenIDs
  , getPOStagTagSet
  , parsePOStags
  , parsePOStag
  , writePOStagLayer
  ) where

import Text.XML.HXT.Core
import Data.Maybe
import Data.List
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import qualified Data.Aeson as A
import Control.Lens

import HTCF.Config
import HTCF.Utils
import HTCF.ArrowXml
import HTCF.Range

-- | This modules defines types and functions for reading and writing
-- the POStag layer.
-- Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#POStags
--
-- The tokenizer is in Tokenizer.hs.


-- * POStag type defs

-- | Represents a single POStag
data POStag = POStag                      
  { _postag_posTag :: String                  -- ^ the POStag
  , _postag_tokenIDs :: [Int]                 -- ^ the IDs of the tokens
  , _postag_tagSet :: Maybe String            -- ^ the tag set
  } deriving (Show, Eq, Generic)

makeLenses ''POStag

-- * Exporting

-- | 'POStag' is ready to be exported to JSON.
instance A.ToJSON POStag

-- | 'POStag' is ready to be exported to CSV.
instance Csv.ToRecord POStag where
  toRecord (POStag tag ids st) =
    Csv.record [ Csv.toField tag
               , Csv.toField $ intercalate " " $ map show ids
               , maybeToField B.empty st
               ]

-- | 'POStag' is ready to be exported to CSV with text and source
-- offsets formatted as PostgreSQL's range type.
instance Csv.ToRecord (PostgresRange POStag) where
  toRecord (PostgresRange ptag)
    = Csv.toRecord ptag
    
-- * Getters for the fields of the 'POStag' record.

getPOStag :: POStag -> String
getPOStag (POStag t _ _) = t
{-# DEPRECATED getPOStag "Use lenses instead!" #-}

getPOStagTokenIDs :: POStag -> [Int]
getPOStagTokenIDs (POStag _ idd _) = idd
{-# DEPRECATED getPOStagTokenIDs "Use lenses instead!" #-}

getPOStagTagSet :: POStag -> Maybe String
getPOStagTagSet (POStag _ _ s) = s
{-# DEPRECATED getPOStagTagSet "Use lenses instead!" #-}

-- * Arrows for reading a tcf posTag layer.

parsePOStags :: (ArrowXml a) => Config -> Int -> Int -> a XmlTree POStag
parsePOStags cfg pfxLen base =
  --traceMsg 1 ("Parsing POStag layer with prefix length " ++ (show pfxLen) ++ " and base " ++ (show base)) >>> 
  isElem >>> hasQNameCase (mkNsName "POStags" $ _cfg_tcfTextCorpusNamespace cfg) >>>
  parseChildren cfg pfxLen base $< getAttrCaseValue "tagset" -- arr (const "sttla")
  where
    parseChildren :: (ArrowXml a) => Config -> Int -> Int -> String -> a XmlTree POStag
    parseChildren cfgg pfxLenn basee tagsett =
      getChildren >>> parsePOStag cfgg pfxLenn basee tagsett
        
parsePOStag :: (ArrowXml a) => Config -> Int -> Int -> String -> a XmlTree POStag
parsePOStag cfg pfxLen base tagset =
  hasQNameCase (mkNsName "tag" $ _cfg_tcfTextCorpusNamespace cfg) >>>
  (getChildren >>> getText) &&&
  getAttrCaseValue "tokenIDs" >>>
  arr (\(tag, ids) ->
         (POStag
           tag
           (parseIDs pfxLen base ids)
           (Just tagset)))
{-# INLINE parsePOStag #-}

-- * Arrows for writing the tcf posTag layer.

-- | Arrow for writing the posTag layer.
writePOStagLayer :: (ArrowXml a) => Config   -- ^ the config
                -> [POStag]                  -- ^ the list of posTags
                -> a XmlTree XmlTree         -- ^ returns an xml arrow
writePOStagLayer cfg ts =
  let base = _cfg_tcfIdBase cfg
      prefix = _cfg_tcfTokenIdPrefix cfg
      ns = _cfg_tcfTextCorpusNamespace cfg in
    (mkqelem
     (mkNsName "POStags" ns) -- qname
     [] -- attribute nodes  % TODO: write tagset
     (map (writePOStag ns prefix base) ts))
  where
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    maybeStrAttr n val = maybeToList $ fmap (sattr n) val
    writePOStag :: (ArrowXml a) => String -> String -> Int -> POStag -> a XmlTree XmlTree
    writePOStag nsuri pfx bs (POStag t ids _) =
      (mkqelem
       (mkNsName "tag" nsuri)
       (maybeAttr "tokenIDs" $ Just $ concatMap ((++ " ") . (fromMaybe "") . (idToBase pfx bs) . Just) ids)
       [(txt t)])
