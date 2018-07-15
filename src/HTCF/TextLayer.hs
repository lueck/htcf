{-# LANGUAGE DisambiguateRecordFields, DeriveGeneric, TemplateHaskell #-}
module HTCF.TextLayer
  ( Text (..)
  , parseTextLayer
  , writeTextLayer
  , getTextText
  ) where

import Text.XML.HXT.Core
import GHC.Generics
import qualified Data.Csv as Csv
import qualified Data.Aeson as A
import Control.Lens

import HTCF.Config
import HTCF.ArrowXml

-- | Cf. http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format#Annotation_Layers

-- * Type defs for the text layer.

-- | Represents the text layer the text.
data Text = Text
  { _text_text :: String
  } deriving (Show, Eq, Generic)

makeLenses ''Text

-- | 'Text' is ready to be exported to CSV.
instance Csv.ToRecord Text

-- | 'Text' is read to be exported to JSON.
instance A.ToJSON Text

-- | Get the text of 'Text'.
getTextText :: Text -> String
getTextText (Text s) = s
{-# DEPRECATED getTextText "Use lenses instead!" #-}


-- * Arrows for reading the tcf text layer. 

parseTextLayer :: (ArrowXml a) => Config -> a XmlTree Text
parseTextLayer cfg =
  isElem >>> hasQNameCase (mkNsName "text" $ _cfg_tcfTextCorpusNamespace cfg) >>>
  getChildren >>>
  isText >>>
  getText >>> arr Text

-- * Arrows for writing the tcf text layer.

-- | Arrow for writing the text layer.
writeTextLayer :: (ArrowXml a) => Config -- ^ the config
               -> String                   -- ^ the content of text layer as string
               -> a XmlTree XmlTree        -- ^ returns an xml arrow
writeTextLayer cfg t =
  (mkqelem
   (mkNsName "text" ns) -- qname <tcf:text>
   [] -- attribute nodes
   [txt t]) -- child nodes: a single text node containing the text
  where
    ns = _cfg_tcfTextCorpusNamespace cfg
