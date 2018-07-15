{-# LANGUAGE TemplateHaskell #-}

module HTCF.Config where

import Text.XML.HXT.Core
import Control.Lens
import Data.Default.Class


-- * Record for application configuration

data Config = Config
  { _cfg_textRoot :: QName      -- ^ qname of parent node for text
  , _cfg_droppedTrees :: [QName] -- ^ xml subtree to be dropped by the
                                 -- parser
  , _cfg_qName :: QName         -- ^ qname of element node
  , _cfg_lineBreaks :: [QName]  -- ^ qname of element resulting in a
                                -- control sign for the tokenizer
  , _cfg_hyphens :: [Char]       -- ^ hyphen characters for tokenizer
  , _cfg_tcfRootNamespace :: String -- ^ the namespace of the root
                                    -- node of a TCF file
  , _cfg_tcfTextCorpusNamespace :: String -- ^ the namespace of the
                                        -- TextCorpus tag of a TCF
                                        -- file
  , _cfg_tcfMetadataNamespace :: String -- ^ the namespace of the meta
                                        -- data (preamble) of a TCF
                                        -- file
  , _cfg_tcfIdBase :: Int               -- ^ The base of the IDs used
                                        -- in a TCF file
  , _cfg_tcfTokenIdPrefix :: String     -- ^ The prefix of the token
                                        -- IDs used in a TCF file
  , _cfg_tcfSentenceIdPrefix :: String -- ^ The prefix of the sentence
                                       -- IDs used in a TCF file
  , _cfg_abbreviations :: [String]     -- ^ An abbreviation (string
                                       -- without ending dot)
  , _cfg_months :: [String]            -- ^ A month. Months are needed
                                       -- for abbreviation and
                                       -- tokenization of days.
  , _cfg_abbrev1CharToken :: Bool      -- ^ Whether tokens with a
                                       -- length of one word followed
                                       -- by a punct are treated as a
                                       -- abbreviation.
  , _cfg_singleDigitOrdinal :: Bool    -- ^ Whether single digits
                                       -- followed by a punct are
                                       -- treated as an ordinal.
  , _cfg_noBreak :: [Char]             -- ^ Characters which by their
                                       -- category result in a break,
                                       -- i.e. punctuation or space,
                                       -- but which is not supposed to
                                       -- do so.
  } deriving (Show, Eq)

makeLenses ''Config


-- * Default configuration values.

instance Default Config where
  def = Config
    { _cfg_textRoot = mkNsName "text" "http://www.tei-c.org/ns/1.0"
    , _cfg_droppedTrees = []
    , _cfg_qName = mkNsName "fail" "http://www.tei-c.org/ns/1.0"
    , _cfg_lineBreaks = [ (mkNsName "lb" "http://www.tei-c.org/ns/1.0") ]
    , _cfg_hyphens = "-"
    , _cfg_tcfRootNamespace = "http://www.dspin.de/data"
    , _cfg_tcfTextCorpusNamespace = "http://www.dspin.de/data/textcorpus"
    , _cfg_tcfMetadataNamespace = "http://www.dspin.de/data/metadata"
    , _cfg_tcfIdBase = 10
    , _cfg_tcfTokenIdPrefix = "t_"
    , _cfg_tcfSentenceIdPrefix = "s_"
    , _cfg_abbreviations = []
    , _cfg_months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    , _cfg_abbrev1CharToken = False
    , _cfg_singleDigitOrdinal = False
    , _cfg_noBreak = []
    }
