{-# LANGUAGE DisambiguateRecordFields #-}
module HTCF.TokenLayer
  ( Token (..)
  , TokenID
  , getToken
  , getTokenID
  , getTokenStartTextPos
  , getTokenEndTextPos
  , getTokenStartSrcPos
  , getTokenEndSrcPos
  , parseTokens
  , parseToken
  , writeTokenLayer
  ) where

import Text.XML.HXT.Core
import Data.Maybe

import HTCF.ConfigParser
import HTCF.Position
import HTCF.Utils

-- | This modules defines types and functions for the reading and
-- writing the token layer.
-- Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Tokens
--
-- The tokenizer is in Tokenizer.hs.


-- * Token type defs

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

-- * Getters for the fields of the 'Token' record.

getToken :: Token -> String
getToken (Token t _ _ _ _ _) = t

getTokenID :: Token -> Maybe TokenID
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

parseTokens :: (ArrowXml a) => [Config] -> a XmlTree Token
parseTokens cfg =
  isElem >>> hasQName (mkNsName "tokens" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  parseToken cfg

parseToken :: (ArrowXml a) => [Config] -> a XmlTree Token
parseToken cfg =
  hasQName (mkNsName "token" $ getTcfTextCorpusNamespace cfg) >>>
  (getChildren >>> getText) &&&
  getAttrValue "ID" &&&
  getAttrValue "start" &&&
  getAttrValue "end" &&&
  getAttrValue "srcStart" &&&
  getAttrValue "srcEnd" >>>
  arr (\(t, (idd, (s, (e, (sS, sE))))) ->
         (Token
           t
           (readBasePrefixed cfg idd)
           (readIntMaybe $ Just s)
           (readIntMaybe $ Just e)
           (readIntMaybe $ Just sS)
           (readIntMaybe $ Just sE)))
{-# INLINE parseToken #-}

-- * Arrows for writing the tcf token layer.

-- | Arrow for writing the token layer.
writeTokenLayer :: (ArrowXml a) => [Config] -- ^ the config
                -> [Token]                  -- ^ the list of tokens
                -> a XmlTree XmlTree        -- ^ returns an xml arrow
writeTokenLayer cfg ts =
  (mkqelem
   (mkNsName "tokens" ns) -- qname
   [] -- attribute nodes
   (map writeToken ts))
  where
    ns = getTcfTextCorpusNamespace cfg
    maybeAttr n val = maybeToList $ fmap ((sattr n) . show) val
    writeToken :: (ArrowXml a) => Token -> a XmlTree XmlTree
    writeToken (Token t idd start end sStart sEnd) =
      (mkqelem
       (mkNsName "token" ns)
       ((maybeAttr "id" idd) ++ (maybeAttr "start" start) ++ (maybeAttr "end" end) ++ (maybeAttr "srcStart" sStart) ++ (maybeAttr "srcEnd" sEnd))
       [(txt t)])
