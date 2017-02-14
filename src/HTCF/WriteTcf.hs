module HTCF.WriteTcf
  ( runTcfWriter
  ) where

import Text.XML.HXT.Core

import HTCF.ConfigParser

-- | Run the tcf writer in the IO monad.
runTcfWriter :: [Config]                              -- ^ the config
             -> FilePath                              -- ^ the output file or \"\" for stdout
             -> [IOSLA (XIOState ()) XmlTree XmlTree] -- ^ arrows for making the preamble
             -> [IOSLA (XIOState ()) XmlTree XmlTree] -- ^ arrows for making the layers
             -> IO [Int]
runTcfWriter cfg fName preamble layers = do
  rc <- runX (mkTcfDocument cfg preamble layers >>>
              writeDocument [withIndent yes] fName >>>
              getErrStatus
             )
  return rc

-- | Arrow for generating the TCF file.
mkTcfDocument :: (ArrowXml a) => [Config]      -- ^ the config
              -> [a XmlTree XmlTree]           -- ^ arrows for making the preamble
              -> [a XmlTree XmlTree]           -- ^ arrows for making the layers
              -> a XmlTree XmlTree             -- ^ returns an xml arrow
mkTcfDocument cfg preamble layers =
  root  -- make a root node containing the XML-Decl, PIs and the xml root element
  []    -- its attribute nodes
  [ -- its child nodes
    (mkqelem  -- make the xml root element
      (mkQName "" "D-Spin" $ getTcfRootNamespace cfg)
      [(sattr "xmlns" $ getTcfRootNamespace cfg)] -- attributes
      [-- its child nodes
        (mkqelem
         (mkQName "" "MetaData" $ getTcfMetadataNamespace cfg)
         [(sattr "xmlns" $ getTcfMetadataNamespace cfg)]
         preamble
        )
      , (mkqelem
         (mkQName "" "textCorpus" $ getTcfTextCorpusNamespace cfg)
         [(sattr "xmlns" $ getTcfTextCorpusNamespace cfg)]
         layers
        )
      ]
    )
  ]
