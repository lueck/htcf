{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe

import HTCF.ReadTcf
import HTCF.TextLayer
import HTCF.TokenLayer
import HTCF.Utils

import HTCF.ConfigParser
import HTCF.TcfParser
import HTCF.TcfParserTypeDefs
import HTCF.Tokenizer

data Convert =
  Convert { configFile :: Maybe String
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
  <*> optional ((flag' Raw (short 'r'
                             <> long "raw"
                             <> help "Output in raw format."))
                <|>
                (flag' PrettyList (short 'u'
                                    <> long "human"
                                    <> help "Output formatted human readable.")))
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile outputMethod inFile) = do
  config <- runConfigParser $ fromMaybe "config.xml" configFile
  layers <- runTcfReader config inFile
  print layers

  
main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Read a TCF file, ie. an xml file in the Text Corpus Format."
            <> header "readtcf - read TCF." )
