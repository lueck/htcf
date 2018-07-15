{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe
import Data.Char
import Control.Lens hiding (argument)
import qualified Data.Csv as C
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as J
import Data.Default.Class

import HTCF.ReadTcf
import HTCF.TcfLayers
import HTCF.TextLayer (getTextText)
import HTCF.Utils
import HTCF.Config
--import HTCF.ConfigParser
import HTCF.TcfParser
import HTCF.TcfParserTypeDefs
import HTCF.Tokenizer
import HTCF.Range

data Convert =
  Convert { configFile :: String
          , readLayer :: Maybe ReadLayer
          , outputMethod :: Maybe OutputMethod
          , csvDelimiter :: String
          , inFile :: String
          }

data OutputMethod = Csv | CsvPgRange | Json | Raw

data ReadLayer = TextLayer | TokenLayer | SentenceLayer | POStagLayer | LemmaLayer 

convert_ :: Parser Convert
convert_ = Convert
  <$> strOption (short 'c'
                  <> long "config"
                  <> help "Specify a config file. Defaults to config.xml in the current directory."
                  <> value "config.xml"
                  <> metavar "CONFIGFILE")
  <*> optional ((flag' TextLayer (short 'x'
                                   <> long "text"
                                   <> help "Get the text layer."))
                <|>
                (flag' TokenLayer (short 't'
                                    <> long "tokens"
                                    <> help "Get the token layer (default)."))
                <|>
                (flag' SentenceLayer (short 's'
                                       <> long "sentences"
                                       <> help "Get the sentence layer."))
                <|>
                (flag' POStagLayer (short 'g'
                                      <> long "postags"
                                      <> help "Get the POStag layer."))
                <|>
                (flag' LemmaLayer (short 'l'
                                     <> long "lemmas"
                                     <> help "Get the lemma layer.")))
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
  let config::Config = def
  --config <- runConfigParser configFile
  layers <- runTcfReader config inFile
  let
    csvOpts = C.defaultEncodeOptions {
      C.encDelimiter = fromIntegral $ ord $ head csvDel
      }
    csv = case readLayer of
            Just TextLayer -> (C.encodeWith csvOpts) . getTextLayer
            Just TokenLayer -> (C.encodeWith csvOpts) . getTokens
            Just SentenceLayer -> (C.encodeWith csvOpts) . getSentences
            Just POStagLayer -> (C.encodeWith csvOpts) . getPOStags
            Just LemmaLayer -> (C.encodeWith csvOpts) . getLemmas
            otherwise -> (C.encodeWith csvOpts) . getTokens
    csvPgRange = case readLayer of
                   Just TextLayer -> (C.encodeWith csvOpts) . getTextLayer
                   Just TokenLayer -> ((C.encodeWith csvOpts) . (map PostgresRange)) . getTokens
                   Just SentenceLayer -> ((C.encodeWith csvOpts) . (map PostgresRange)) . getSentences
                   Just POStagLayer -> (C.encodeWith csvOpts) . getPOStags
                   Just LemmaLayer -> (C.encodeWith csvOpts) . getLemmas
                   otherwise -> ((C.encodeWith csvOpts) . (map PostgresRange)) . getTokens
    json = case readLayer of
             Just TextLayer -> J.encode . getTextLayer
             Just TokenLayer -> J.encode . getTokens
             Just SentenceLayer -> J.encode . getSentences
             Just POStagLayer -> J.encode . getPOStags
             Just LemmaLayer -> J.encode . getLemmas
             otherwise -> J.encode . getTokens
    raw = case readLayer of
            Just TextLayer -> (getTextText . head) . getTextLayer
            Just TokenLayer -> show . getTokens
            Just SentenceLayer -> show . getSentences
            Just POStagLayer -> show . getPOStags
            Just LemmaLayer -> show . getLemmas
            otherwise -> show . getTokens
  case outputMethod of
    Just Csv -> B.putStr $ csv layers -- no new line at the end
    Just CsvPgRange -> B.putStr $ csvPgRange layers
    Just Json -> B.putStrLn $ json layers
    Just Raw -> putStrLn $ raw layers
    otherwise -> B.putStr $ csv layers
  {-
  -- This would be shorter but results in a compile error
  let
    method = case outputMethod of
               Just Csv -> B.putStrLn . (C.encodeWith csvOpts)
               Just CsvPgRange -> B.putStr . (C.encodeWith csvOpts) . (map PostgresRange)
               Just Json -> B.putStrLn . J.encode
               otherwise -> B.putStrLn . (C.encodeWith csvOpts)
  case readLayer of
    Just TokenLayer -> method $ getTokens layers
    Just TextLayer -> method $ getTextLayer layers
    otherwise -> method $ getTokens layers
  -}
  


main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Get a layer of a TCF file, ie. an XML file in the Text Corpus Format. Format the output to CSV, JSON etc."
            <> header "tcflayer - get a layer of a TCF file." )
