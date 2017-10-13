{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module HTCF.TokenLayer
  ( Token (..)
  , getToken
  , getTokenID
  , getTokenStartTextPos
  , getTokenEndTextPos
  , getTokenStartSrcPos
  , getTokenEndSrcPos
  , parseTokens
  , parseToken
  , guessAboutTokenId
  , writeTokenLayer
  ) where

import Text.XML.HXT.Core
import Data.Maybe
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
-- the token layer.
-- Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Tokens
--
-- The tokenizer is in Tokenizer.hs.


-- * Token type defs

-- | Represents a single token
data Token = Token                      
  { token :: String                   -- ^ the token
  , tokenId :: Maybe Int              -- ^ the token's ID
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

-- | 'Token' is read to be exported to JSON.
instance A.ToJSON Token

-- | 'Token' is ready to be exported to CSV.
instance Csv.ToRecord Token

-- | 'Token' is ready to be exported to CSV with text and source
-- offsets formatted as PostgreSQL's range type.
instance Csv.ToRecord (PostgresRange Token) where
  toRecord (PostgresRange (Token token tokenId start end srcStart srcEnd))
    = Csv.record [ Csv.toField token
                 , toField' B.empty tokenId
                 , (B.concat [ "[", (toField' "NULL" start), ","
                             , (toField' "NULL" end), "]"])
                 , (B.concat [ "[", (toField' "NULL" srcStart), ","
                             , (toField' "NULL" srcEnd), "]"])
                 ]

toField' :: (Csv.ToField a) => B.ByteString -- ^ Default value
         -> Maybe a -- ^ the maybe field, 
         -> Csv.Field
toField' deflt f = maybe deflt Csv.toField f 
   

-- * Getters for the fields of the 'Token' record.

getToken :: Token -> String
getToken (Token t _ _ _ _ _) = t

getTokenID :: Token -> Maybe Int
getTokenID (Token _ idd _ _ _ _) = idd

getTokenStartTextPos :: Token -> Maybe TextPosition
getTokenStartTextPos (Token _ _ s _ _ _) = s

getTokenEndTextPos :: Token -> Maybe TextPosition
getTokenEndTextPos (Token _ _ _ e _ _) = e

getTokenStartSrcPos :: Token -> Maybe XmlPosition
getTokenStartSrcPos (Token _ _ _ _ s _) = s

getTokenEndSrcPos :: Token -> Maybe XmlPosition
getTokenEndSrcPos (Token _ _ _ _ _ e) = e

-- * Arrows for reading a tcf token layer.

parseTokens :: (ArrowXml a) => [Config] -> Int -> Int -> a XmlTree Token
parseTokens cfg pfxLen base =
  --traceMsg 1 ("Parsing token layer with prefix length " ++ (show pfxLen) ++ " and base " ++ (show base)) >>> 
  isElem >>> hasQNameCase (mkNsName "tokens" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  parseToken cfg pfxLen base

parseToken :: (ArrowXml a) => [Config] -> Int -> Int -> a XmlTree Token
parseToken cfg pfxLen base =
  hasQNameCase (mkNsName "token" $ getTcfTextCorpusNamespace cfg) >>>
  (getChildren >>> getText) &&&
  getAttrCaseValue "ID" &&&
  getAttrCaseValue "start" &&&
  getAttrCaseValue "end" &&&
  getAttrCaseValue "srcStart" &&&
  getAttrCaseValue "srcEnd" >>>
  arr (\(t, (idd, (s, (e, (sS, sE))))) ->
         (Token
           t
           (readBase base $ drop pfxLen idd)
           (readIntMaybe $ Just s)
           (readIntMaybe $ Just e)
           (readIntMaybe $ Just sS)
           (readIntMaybe $ Just sE)))
{-# INLINE parseToken #-}

guessAboutTokenId :: [Config] -> XmlTrees -> IO (Int, Int)
guessAboutTokenId cfg tree = do
  ids <- runX (constL tree //>
               multi (isElem >>> hasQNameCase (mkNsName "token" $ getTcfTextCorpusNamespace cfg) >>>
               getAttrCaseValue "ID"))
  let pfxLen = length $ commonPrefix $ take 32 $ filter (/= "") ids
  return (pfxLen, (guessBase $ map (drop pfxLen) ids))

-- * Arrows for writing the tcf token layer.

-- | Arrow for writing the token layer.
writeTokenLayer :: (ArrowXml a) => [Config] -- ^ the config
                -> [Token]                  -- ^ the list of tokens
                -> a XmlTree XmlTree        -- ^ returns an xml arrow
writeTokenLayer cfg ts =
  let base = getTcfIdBase cfg
      prefix = getTcfTokenIdPrefix cfg
      ns = getTcfTextCorpusNamespace cfg in
    (mkqelem
     (mkNsName "tokens" ns) -- qname
     [] -- attribute nodes
     (map (writeToken ns prefix base) ts))
  where
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    maybeStrAttr n val = maybeToList $ fmap (sattr n) val
    writeToken :: (ArrowXml a) => String -> String -> Int -> Token -> a XmlTree XmlTree
    writeToken nsuri pfx bs (Token t idd start end sStart sEnd) =
      (mkqelem
       (mkNsName "token" nsuri)
       ((maybeStrAttr "id" (idToBase pfx bs idd)) ++
        (maybeAttr "start" start) ++
        (maybeAttr "end" end) ++
        (maybeAttr "srcStart" sStart) ++
        (maybeAttr "srcEnd" sEnd))
       [(txt t)])
