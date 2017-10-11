{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe
import Data.Char
import Control.Lens hiding (argument)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Csv as C
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as J

import HTCF.TcfData
import HTCF.ReadTcf
import HTCF.ConfigParser

data Convert =
  Convert { configFile :: String
          , readLayer :: Maybe ReadLayer
          , outputMethod :: Maybe OutputMethod
          , csvDelimiter :: String
          , inFile :: String
          }

data OutputMethod = Csv | CsvPgRange | Json | Raw

data ReadLayer = NoTokenLayer | NoSentenceLayer | NoPOStagLayer | NoLemmaLayer 

convert_ :: Parser Convert
convert_ = Convert
  <$> strOption (short 'c'
                  <> long "config"
                  <> help "Specify a config file. Defaults to config.xml in the current directory."
                  <> value "config.xml"
                  <> metavar "CONFIGFILE")
  <*> optional ((flag' NoTokenLayer (short 'T'
                                     <> long "no-tokens"
                                     <> help "Do not get the token layer (default)."))
                <|>
                (flag' NoSentenceLayer (short 'S'
                                        <> long "no-sentences"
                                        <> help "Do not get the sentence layer."))
                <|>
                (flag' NoPOStagLayer (short 'G'
                                       <> long "no-postags"
                                       <> help "Do not get the POStag layer."))
                <|>
                (flag' NoLemmaLayer (short 'L'
                                     <> long "no-lemmas"
                                     <> help "Do not get the lemma layer.")))
  <*> optional ((flag' Csv (short 'v'
                             <> long "csv"
                             <> help "Output as comma separated values (CSV). This is the default output format."))
                <|>
                (flag' CsvPgRange (short 'p'
                                   <> long "csv-pg-range"
                                   <> help "Output as comma separated values (CSV), but format text offsets and source offsets as PostgreSQL's range type, i.e. '...,\"[textStart,textEnd]\",\"[sourceStart,sourceEnd]\",...'. Since the range contains a comma, it is quoted, if the CSV delimiter is set to comma (default)."))

                <|>
                (flag' Json (short 'j'
                              <> long "json"
                              <> help "Output as JavaScript Object Notation (JSON)."))
                <|>
                (flag' Raw (short 'r'
                             <> long "raw"
                             <> help "Output in raw haskell format. Choose this format to get the text layer as string.")))
  <*> strOption (long "csv-delimiter"
                  <> help "Delimiter for CSV output. Defaults to ',' (comma)."
                  <> value ","
                  <> metavar "CHAR")
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile readLayer outputMethod csvDel inFile) = do
  config <- runConfigParser configFile
  layers <- runTcfReader config inFile
  let
    csvOpts = C.defaultEncodeOptions {
      C.encDelimiter = fromIntegral $ ord $ head csvDel
      }
    method = case outputMethod of
      Just Csv -> (C.encodeWith csvOpts)
      Just Json -> J.encode
      otherwise -> (C.encodeWith csvOpts)
  B.putStrLn $ method $ IntMap.elems $ collectTokenData layers

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Get a layer of a TCF file, ie. an XML file in the Text Corpus Format. Format the output to CSV, JSON etc."
            <> header "tcflayer - get a layer of a TCF file." )
