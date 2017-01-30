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
                  hasName "text" >>>
                  multi (parserArrow config)
                 )
  writeOut ((addAbbreviations (lines abbrevs) config), parsed)
  where
    writeOut :: ([Config], [TcfElement]) -> IO ()
    writeOut = case outputMethod of
          Just Raw -> print . (uncurry (\_ -> propagateOffsets))
          Just PrettyList -> putStrLn . concatMap serialize . (uncurry (\_ -> propagateOffsets))
          otherwise -> print . (uncurry (\cfg -> tokenize cfg . propagateOffsets))
    tokWithOffsets cfg p = tokenize cfg $ propagateOffsets p
    parserArrow
      | outputStructure = mkTcfElement
      | otherwise = mkTcfElementButStructure

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> convert_)
          ( fullDesc
            <> progDesc "Convert an XML file to TCF, the Text Corpus Format."
            <> header "xml2tcf - convert an XML file to TCF." )
