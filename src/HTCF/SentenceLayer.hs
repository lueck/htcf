{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module HTCF.SentenceLayer where

import Text.XML.HXT.Core
import Data.Maybe
import Data.List
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import qualified Data.Aeson as A

import HTCF.ConfigParser
import HTCF.Position
import HTCF.Utils
import HTCF.ArrowXml
import HTCF.Range

-- | This modules defines types and functions for reading and writing
-- the sentence layer.
-- Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Sentences

-- * Sentence type defs

type SentenceID = Int

-- | Represents a single sentence
data Sentence = Sentence                      
  { tokens :: [Int]                   -- ^ the IDs of the tokens in the sentence
  , sentenceId :: Maybe SentenceID    -- ^ the sentence's ID
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
    } deriving (Show, Eq, Generic)

-- * Exporting

-- | 'Sentence' is read to be exported to JSON.
instance A.ToJSON Sentence

-- | 'Sentence' is ready to be exported to CSV.
instance Csv.ToRecord Sentence where
  toRecord (Sentence ids sentenceId start end srcStart srcEnd)
    = Csv.record [ Csv.toField $ intercalate " " $ map show ids
                 , toField' B.empty sentenceId
                 , toField' B.empty start
                 , toField' B.empty end
                 , toField' B.empty srcStart
                 , toField' B.empty srcEnd
                 ]

-- | 'Sentence' is ready to be exported to CSV with text and source
-- offsets formatted as PostgreSQL's range type.
instance Csv.ToRecord (PostgresRange Sentence) where
  toRecord (PostgresRange (Sentence ids sentenceId start end srcStart srcEnd))
    = Csv.record [ Csv.toField $ intercalate " " $ map show ids
                 , toField' B.empty sentenceId
                 , (B.concat [ "[", (toField' "NULL" start), ","
                             , (toField' "NULL" end), "]"])
                 , (B.concat [ "[", (toField' "NULL" srcStart), ","
                             , (toField' "NULL" srcEnd), "]"])
                 ]

toField' :: (Csv.ToField a) => B.ByteString -- ^ Default value
         -> Maybe a -- ^ the maybe field, 
         -> Csv.Field
toField' deflt f = maybe deflt Csv.toField f
   

-- * Getters for the fields of the 'Sentence' record.

getTokens :: Sentence -> [Int]
getTokens (Sentence ts _ _ _ _ _) = ts

getSentenceID :: Sentence -> Maybe SentenceID
getSentenceID (Sentence _ idd _ _ _ _) = idd

getSentenceStartTextPos :: Sentence -> Maybe TextPosition
getSentenceStartTextPos (Sentence _ _ s _ _ _) = s

getSentenceEndTextPos :: Sentence -> Maybe TextPosition
getSentenceEndTextPos (Sentence _ _ _ e _ _) = e

getSentenceStartSrcPos :: Sentence -> Maybe XmlPosition
getSentenceStartSrcPos (Sentence _ _ _ _ s _) = s

getSentenceEndSrcPos :: Sentence -> Maybe XmlPosition
getSentenceEndSrcPos (Sentence _ _ _ _ _ e) = e

-- * Arrows for reading a tcf sentence layer.

-- | A arrow for parsing the sentences given in a TCF file.
parseSentences :: (ArrowXml a) => [Config] -> Int -> Int -> Int -> Int -> a XmlTree Sentence
parseSentences cfg sentPfxLen sentBase tokPfxLen tokBase =
  --traceMsg 1 ("Parsing sentence layer with prefix length " ++ (show pfxLen) ++ " and base " ++ (show base)) >>> 
  isElem >>> hasQName (mkNsName "sentences" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  parseSentence cfg sentPfxLen sentBase tokPfxLen tokBase

parseSentence :: (ArrowXml a) => [Config] -> Int -> Int -> Int -> Int -> a XmlTree Sentence
parseSentence cfg sentPfxLen sentBase tokPfxLen tokBase =
  hasQName (mkNsName "sentence" $ getTcfTextCorpusNamespace cfg) >>>
  getAttrCaseValue "tokenIDs" &&&
  getAttrCaseValue "ID" &&&
  getAttrCaseValue "start" &&&
  getAttrCaseValue "end" &&&
  getAttrCaseValue "srcStart" &&&
  getAttrCaseValue "srcEnd" >>>
  arr (\(t, (idd, (s, (e, (sS, sE))))) ->
         (Sentence
           (parseIDs tokPfxLen tokBase t)
           (readBase sentBase $ drop sentPfxLen idd)
           (readIntMaybe $ Just s)
           (readIntMaybe $ Just e)
           (readIntMaybe $ Just sS)
           (readIntMaybe $ Just sE)))
{-# INLINE parseSentence #-}


guessAboutSentenceId :: [Config] -> XmlTrees -> IO (Int, Int)
guessAboutSentenceId cfg tree = do
  ids <- runX (constL tree //>
               multi (isElem >>> hasQName (mkNsName "sentence" $ getTcfTextCorpusNamespace cfg) >>>
               getAttrCaseValue "ID"))
  let pfxLen = length $ commonPrefix $ take 32 $ filter (/= "") ids
  return (pfxLen, (guessBase $ map (drop pfxLen) ids))

-- * Arrows for writing the tcf sentence layer.

-- | Arrow for writing the sentence layer.
writeSentenceLayer :: (ArrowXml a) => [Config] -- ^ the config
                -> [Sentence]                  -- ^ the list of sentences
                -> a XmlTree XmlTree        -- ^ returns an xml arrow
writeSentenceLayer cfg ts =
  let sentBase = getTcfIdBase cfg
      sentPrefix = getTcfSentenceIdPrefix cfg
      tokBase = getTcfIdBase cfg
      tokPrefix = getTcfTokenIdPrefix cfg
      ns = getTcfTextCorpusNamespace cfg in
    (mkqelem
     (mkNsName "sentences" ns) -- qname
     [] -- attribute nodes
     (map (writeSentence ns sentPrefix sentBase tokPrefix tokBase) ts))
  where
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    maybeStrAttr n val = maybeToList $ fmap (sattr n) val
    writeSentence :: (ArrowXml a) => String -> String -> Int -> String -> Int -> Sentence -> a XmlTree XmlTree
    writeSentence nsuri sentPfx sentBs tokPfx tokBs (Sentence toks idd start end sStart sEnd) =
      (mkqelem
       (mkNsName "sentence" nsuri)
       ((maybeStrAttr "id" (idToBase sentPfx sentBs idd)) ++
        (maybeAttr "start" start) ++
        (maybeAttr "end" end) ++
        (maybeAttr "srcStart" sStart) ++
        (maybeAttr "srcEnd" sEnd) ++
        (maybeAttr "tokenIDs" $ Just $ concatMap ((++ " ") . (fromMaybe "") . (idToBase tokPfx tokBs) . Just) toks))
       [])
