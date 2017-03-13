{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Data.Maybe
import Control.Lens hiding (argument)
import qualified Data.Csv as C
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as J

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
          , readLayer :: Maybe ReadLayer
          , outputMethod :: Maybe OutputMethod
          , inFile :: String
          }

data OutputMethod = Csv | Json | Raw

data ReadLayer = TextLayer | TokenLayer

convert_ :: Parser Convert
convert_ = Convert
  <$> optional (strOption (short 'c'
                            <> long "config"
                            <> help "Specify a config file."
                            <> metavar "CONFIGFILE" ))
  <*> optional ((flag' TextLayer (short 'x'
                                   <> long "text"
                                   <> help "Get the text layer."))
                <|>
                (flag' TokenLayer (short 't'
                                    <> long "tokens"
                                    <> help "Get the token layer (default).")))
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
                             <> help "Output in raw haskell format. Choose this format to get the text layer as string.")))
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile readLayer outputMethod inFile) = do
  config <- runConfigParser $ fromMaybe "config.xml" configFile
  layers <- runTcfReader config inFile
  let
    csv = case readLayer of
            Just TokenLayer -> layers^._2.to C.encode
            Just TextLayer -> layers^._1.to C.encode
            otherwise -> layers^._2.to C.encode
    json = case readLayer of
             Just TokenLayer -> layers^._2.to J.encode
             Just TextLayer -> layers^._1.to J.encode
             otherwise -> layers^._2.to J.encode
    raw = case readLayer of
            Just TokenLayer -> layers^._2.to show
            Just TextLayer -> layers^._1.to (getTextText . head)
            otherwise -> layers^._2.to show
  case outputMethod of
    Just Csv -> B.putStr csv -- no new line at the end
    Just Json -> B.putStrLn json
    Just Raw -> putStrLn raw
    otherwise -> B.putStr csv
  {- -- This would be shorter but results in a compile error
  let
    method = case outputMethod of
               Just Csv -> B.putStrLn . C.encode
               Just Json -> B.putStrLn . J.encode
               otherwise -> B.putStrLn . C.encode
  case readLayer of
    Just TokenLayer -> layers ^._2.to method
    Just TextLayer -> layers ^._1.to method
    otherwise -> layers ^._2.to method
  -}


main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Get a layer of a TCF file, ie. an XML file in the Text Corpus Format. Format the output to CSV, JSON etc."
            <> header "tcflayer - get a layer of a TCF file." )
