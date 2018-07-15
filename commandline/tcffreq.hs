{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
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
import Data.Default.Class

import HTCF.TcfData
import HTCF.ReadTcf
import HTCF.Config
--import HTCF.ConfigParser
import HTCF.TcfLayers

data Convert =
  Convert { configFile :: String
          , readLayer :: Maybe ReadLayer
          , outputMethod :: Maybe OutputMethod
          , csvDelimiter :: String
          , inFile :: String
          }

data OutputMethod = Csv | Json | Raw

data ReadLayer = TokenLayer | LemmaLayer 

convert_ :: Parser Convert
convert_ = Convert
  <$> strOption (short 'c'
                  <> long "config"
                  <> help "Specify a config file. Defaults to config.xml in the current directory."
                  <> value "config.xml"
                  <> metavar "CONFIGFILE")
  <*> optional ((flag' TokenLayer (short 't'
                                     <> long "tokens"
                                     <> help "Calculate the frequencies of tokens (default)."))
                <|>
                (flag' LemmaLayer (short 'l'
                                     <> long "lemmas"
                                     <> help "Calcualte the frequencies of lemmas.")))
  <*> optional ((flag' Csv (short 'v'
                             <> long "csv"
                             <> help "Output as comma separated values (CSV). This is the default output format."))
                <|>
                (flag' Json (short 'j'
                              <> long "json"
                              <> help "Output as JavaScript Object Notation (JSON)."))
                <|>
                (flag' Raw (short 'r'
                             <> long "raw"
                             <> help "Output in raw haskell format.")))
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
    method = case outputMethod of
      Just Csv -> B.putStr . (C.encodeWith csvOpts)
      Just Json -> B.putStrLn . J.encode
      otherwise -> B.putStr . (C.encodeWith csvOpts)
    freqs = case readLayer of
      -- Just LemmaLayer -> map getToken $ tokensFromLemmas $ getLemmas layers
      -- otherwise -> map getLemma $ map fromToken $ getTokens layers
      Just LemmaLayer -> frequencies getLemma $ tokensFromLemmas $ getLemmas layers
      otherwise -> frequencies getToken $ map fromToken $ getTokens layers
  method freqs
  --print freqs

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Calculate the relative frequencies of tokens or lemmas given in a TCF file, ie. an XML file in the Text Corpus Format. Format the output to CSV, JSON etc."
            <> header "tcffreq - calculate token frequencies." )
