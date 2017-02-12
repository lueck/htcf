{-# LANGUAGE DisambiguateRecordFields #-}
module HTCF.TextLayer
  ( Text (..)
  , parseTextLayer
  , writeTextLayer
  ) where

import Text.XML.HXT.Core

import HTCF.ConfigParser

-- | Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Annotation_Layers

-- * Type defs for the text layer.

-- | Represents the text layer the text.
data Text = Text String
  deriving (Show, Eq)

-- * Arrows for reading the tcf text layer. 

parseTextLayer :: (ArrowXml a) => [Config] -> a XmlTree Text
parseTextLayer cfg =
  isElem >>> hasQName (mkNsName "text" $ getTcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  isText >>>
  getText >>> arr Text

-- * Arrows for writing the tcf text layer.

-- | Arrow for writing the text layer.
writeTextLayer :: (ArrowXml a) => [Config] -- ^ the config
               -> String                   -- ^ the content of text layer as string
               -> a XmlTree XmlTree        -- ^ returns an xml arrow
writeTextLayer cfg t =
  (mkqelem
   (mkNsName "text" ns) -- qname <tcf:text>
   [] -- attribute nodes
   [txt t]) -- child nodes: a single text node containing the text
  where
    ns = getTcfTextCorpusNamespace cfg
