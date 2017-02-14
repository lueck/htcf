{-# LANGUAGE CPP, FlexibleContexts #-}
module Main where

import System.IO
import System.Exit
import Options.Applicative
import Data.Monoid ((<>))
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runXIOState, initialState)
import Data.Maybe

import HTCF.ConfigParser
import HTCF.TcfParser
import HTCF.TcfParserTypeDefs
import HTCF.TokenLayer
import HTCF.Tokenizer
import HTCF.TextLayer
import HTCF.StructureLayer
import HTCF.ArrowXml
import HTCF.WriteTcf
import HTCF.LineOffsets

import qualified HTCF.PosParser.ReadDocument as RD (readDocument)

data Convert =
  Convert { configFile :: Maybe String
          , abbrevFile :: Maybe FilePath
          , outputStructure :: Bool
          , outFile :: Maybe String
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
  <*> flag True False (short 'S'
                       <> long "no-structure"
                       <> help "Do not output structure layer.")
  <*> optional (strOption (short 'o'
                            <> long "output"
                            <> help "Output file. If left, the TCF is printed to stdout."
                            <> metavar "OUTFILE" ))
  <*> argument str (metavar "INFILE")

run :: Convert -> IO ()
run (Convert configFile abbrevFile outputStructure outFile fName) = do
  config <- runConfigParser $ fromMaybe "config.xml" configFile
  abbrevs <- readFile $ fromMaybe "abbrevs.txt" abbrevFile
  lineOffsets <- runLineOffsetParser fName
  parsed <- runXIOState (initialState lineOffsets)
            (RD.readDocument [ withValidate no
                             , withCanonicalize no -- do not substitute char refs
                             --, withTrace 4
                             ] fName >>>
             propagateNamespaces >>>
             stripQNames (getDroppedTrees config) //>
             hasQName (getTextRoot config) >>>
             multi (parserArrow config)
            )
  let
    parsedOffsets = propagateOffsets parsed
    tokens = tokenize (addAbbreviations (lines abbrevs) config) parsedOffsets
    textLayer = writeTextLayer config $
                concatMap getTcfText parsed
    tokenLayer = writeTokenLayer config tokens
    structureLayer = writeTextStructureLayer config $
                     mkTextSpans tokens $ filter isTcfStructure parsedOffsets
  [rc] <- runTcfWriter config (fromMaybe "" outFile) [{-preamble-}] [textLayer, tokenLayer, structureLayer]
  exitWith (if rc >= c_err
             then ExitFailure 1
             else ExitSuccess
           )
  where
    parserArrow
      | outputStructure = mkTcfElement
      | otherwise = mkTcfElementButStructure

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "xml2tcf generates a TCF file from XML input. TCF is the Text Corpus Format defined for WebLicht."
            <> header "xml2tcf - generate TCF from XML input." )
