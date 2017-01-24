{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe

import HTCF.ReadTcf
import HTCF.Layers
import HTCF.Utils

import Text.XML.TCF.Parser.ConfigParser
import Text.XML.TCF.Parser.TcfLayerParser
import Text.XML.TCF.Parser.TcfElement
import Text.XML.TCF.Parser.Tokenizer

data Convert =
  Convert { configFile :: Maybe String
          , idBase :: Maybe String
          , prefixLength :: Maybe String
          , outputMethod :: Maybe OutputMethod
          , inFile :: String
          }

data OutputMethod = Raw | PrettyList

convert_ :: Parser Convert
convert_ = Convert
  <$> optional (strOption (short 'c'
                            <> long "config"
                            <> help "Specify a config file."
                            <> metavar "CONFIGFILE" ))
  <*> optional (strOption (short 'b'
                            <> long "id-base"
                            <> help "Base of the IDs in the TCF input file. This overrides the value from the config file."
                            <> metavar "BASE"))
  <*> optional (strOption (short 'l'
                            <> long "id-prefix-length"
                            <> help "Length of the prefix before the numerical part of the IDs in the TCF input file. This overrides the value from the config file."
                            <> metavar "INTEGER"))
  <*> optional ((flag' Raw (short 'r'
                             <> long "raw"
                             <> help "Output in raw format."))
                <|>
                (flag' PrettyList (short 'u'
                                    <> long "human"
                                    <> help "Output formatted human readable.")))
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile bs pxLen outputMethod inFile) = do
  config <- runConfigParser $ fromMaybe "config.xml" configFile
  layers <- runTcfReader (suppConfig config) inFile
  print layers
  where
    -- maybeFun returns id or a closure which takes a config
    suppConfig cfg = maybeFun id setTcfIdPrefixLength (readIntMaybe pxLen) $
                     maybeFun id setTcfIdBase (readIntMaybe bs) cfg
  
  
main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Read a TCF file, ie. an xml file in the Text Corpus Format."
            <> header "readtcf - read TCF." )
