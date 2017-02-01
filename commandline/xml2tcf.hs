{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe

import HTCF.ConfigParser
import HTCF.TcfParser
import HTCF.TcfParserTypeDefs
import HTCF.LayerTypeDefs
import HTCF.Tokenizer
import HTCF.ArrowXml
import HTCF.WriteTcf

data Convert =
  Convert { configFile :: Maybe String
          , abbrevFile :: Maybe FilePath
          , outputMethod :: Maybe OutputMethod
          , outputStructure :: Bool
          , inFile :: String
          }

data OutputMethod = Raw | PrettyList

convert_ :: Parser Convert
convert_ = Convert
  <$> optional (strOption (short 'c'
                            <> long "config"
                            <> help "Specify a config file. Defaults to config.xml in the working directory."
                            <> metavar "CONFIGFILE" ))
  <*> optional (strOption (short 'a'
                            <> long "abbrevs"
                            <> help "Specify a abbreviations file. The file is expected to be plain text with one abbreviation per line. Dots shoult not be in there. Defaults to abbrevs.txt in the working directory."
                            <> metavar "ABBREVFILE" ))
  <*> optional ((flag' Raw (short 'r'
                             <> long "raw"
                             <> help "Output in raw format."))
                <|>
                (flag' PrettyList (short 'u'
                                    <> long "human"
                                    <> help "Output formatted human readable.")))
  <*> flag True False (short 'S'
                       <> long "-no-structure"
                       <> help "Do not output structure layer.")
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile abbrevFile outputMethod outputStructure fName) = do
  config <- runConfigParser $ fromMaybe "config.xml" configFile
  abbrevs <- readFile $ fromMaybe "abbrevs.txt" abbrevFile
  parsed <- runX (readDocument [withValidate no] fName >>>
                  propagateNamespaces >>>
                  stripQNames (getDroppedTrees config) //>
                  hasQName (getTextRoot config) >>>
                  multi (parserArrow config)
                 )
  rc <- runX (root  -- make a root node containing the XML-Decl, PIs and the xml root element
              []    -- its attribute nodes
              [ -- its child nodes
                (mkqelem  -- make the xml root element
                 (mkQName "tcf" "D-Spin" $ getTcfRootNamespace config)
                 [(sattr "xmlns:tcf" $ getTcfRootNamespace config),
                  (sattr "xmlns" $ getTcfTextCorpusNamespace config)] -- attributes
                 [-- its child nodes
                   (mkqelem
                    (mkQName "" "textCorpus" $ getTcfTextCorpusNamespace config)
                    [] -- attributes
                    [ -- its child nodes
                      (writeTextLayer config $ concatMap getTcfText parsed),
                      (writeTokenLayer config $
                       tokenize (addAbbreviations (lines abbrevs) config) $
                       propagateOffsets parsed)])])] >>>
              {-attachNsEnv (toNsEnv [("tcf", getTcfRootNamespace config),
                                    ("", getTcfTextCorpusNamespace config)]) >>> -- FIXME-}
              writeDocumentToString [withIndent yes])
  putStrLn $ concat rc
  where
    parserArrow
      | outputStructure = mkTcfElement
      | otherwise = mkTcfElementButStructure

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Convert an XML file to TCF, the Text Corpus Format."
            <> header "xml2tcf - convert an XML file to TCF." )
